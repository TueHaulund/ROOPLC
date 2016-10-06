{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CodeGenerator (generatePISA) where

import Data.List

import Control.Monad.State
import Control.Monad.Except
import Control.Arrow

import AST
import PISA
import ClassAnalyzer
import ScopeAnalyzer

type Size = Integer

data CGState =
    CGState {
        labelIndex :: SIdentifier,
        registerIndex :: Integer,
        labelTable :: [(SIdentifier, Label)],
        registerStack :: [(SIdentifier, Register)],
        saState :: SAState
    } deriving (Show, Eq)

newtype CodeGenerator a = CodeGenerator { runCG :: StateT CGState (Except String) a }
    deriving (Functor, Applicative, Monad, MonadState CGState, MonadError String)

initialState :: SAState -> CGState
initialState s = CGState { labelIndex = 0, registerIndex = 4, labelTable = [], registerStack = [], saState = s }

registerZero :: Register
registerZero = Reg 0

registerSP :: Register
registerSP = Reg 1

registerRO :: Register
registerRO = Reg 2

registerThis :: Register
registerThis = Reg 3

pushRegister :: SIdentifier -> CodeGenerator Register
pushRegister i =
    do ri <- gets registerIndex
       modify $ \s -> s { registerIndex = 1 + ri, registerStack = (i, Reg ri) : registerStack s }
       return $ Reg ri

popRegister :: CodeGenerator ()
popRegister = modify $ \s -> s { registerIndex = (-1) + registerIndex s, registerStack = drop 1 $ registerStack s }

tempRegister :: CodeGenerator Register
tempRegister =
    do ri <- gets registerIndex
       modify $ \s -> s { registerIndex = 1 + ri }
       return $ Reg ri

popTempRegister :: CodeGenerator ()
popTempRegister = modify $ \s -> s { registerIndex = (-1) + registerIndex s }

lookupRegister :: SIdentifier -> CodeGenerator Register
lookupRegister i = gets registerStack >>= \rs ->
    case lookup i rs of
        Nothing -> throwError $ "ICE: No register reserved for index " ++ show i
        (Just r) -> return r

getMethodName :: SIdentifier -> CodeGenerator MethodName
getMethodName i = gets (symbolTable . saState) >>= \st ->
    case lookup i st of
        (Just (Method _ n)) -> return n
        _ -> throwError $ "ICE: Invalid method index " ++ show i

insertMethodLabel :: SIdentifier -> CodeGenerator ()
insertMethodLabel m =
    do n <- getMethodName m
       i <- gets labelIndex
       modify $ \s -> s { labelIndex = 1 + i, labelTable = (m, "l_" ++ n ++ "_" ++ show i) : labelTable s }

getMethodLabel :: SIdentifier -> CodeGenerator Label
getMethodLabel m = gets labelTable >>= \lt ->
    case lookup m lt of
        (Just l) -> return l
        Nothing -> insertMethodLabel m >> getMethodLabel m

getUniqueLabel :: Label -> CodeGenerator Label
getUniqueLabel l =
    do i <- gets labelIndex
       modify $ \s -> s { labelIndex = 1 + i }
       return $ l ++ "_" ++ show i

getClassSize :: TypeName -> CodeGenerator Size
getClassSize tp = gets (classSize . caState . saState) >>= \cs ->
    case lookup tp cs of
        (Just s) -> return s
        Nothing -> throwError $ "ICE: Unknown class " ++ tp

loadVariable :: SIdentifier -> CodeGenerator (Register, [(Maybe Label, MInstruction)], [CodeGenerator ()])
loadVariable i = lookup i <$> gets (symbolTable . saState) >>= maybe invalid dereference
    where invalid = throwError $ "ICE: Invalid variable index " ++ show i
          dereference (Method _ _) = invalid
          dereference (ClassField _ _ _ o) =
              do ro <- tempRegister
                 rf <- tempRegister
                 let load = [(Nothing, ADD ro registerThis), (Nothing, ADDI ro $ Immediate o), (Nothing, EXCH rf ro)]
                 return (rf, load, [popTempRegister, popTempRegister])
          dereference _ =
              do r <- lookupRegister i
                 rt <- tempRegister
                 return (rt, [(Nothing, EXCH rt r)], [popTempRegister])

cgExpression :: SExpression -> CodeGenerator (Register, [(Maybe Label, MInstruction)], [CodeGenerator ()])
cgExpression (Constant 0) = return (registerZero, [], [])
cgExpression (Constant n) = tempRegister >>= \rt -> return (rt, [(Nothing, XORI rt $ Immediate n)], [popTempRegister])
cgExpression (Variable i) = loadVariable i
cgExpression Nil = return (registerZero, [], [])
cgExpression (Binary op e1 e2) =
    do (r1, l1, u1) <- cgExpression e1
       (r2, l2, u2) <- cgExpression e2
       rt <- tempRegister
       return (rt, l1 ++ l2 ++ [(Nothing, XOR rt r1), (Nothing, cgOp op rt r2)], popTempRegister : u2 ++ u1)
    where cgOp Add = ADD
          cgOp Sub = SUB
          cgOp Xor = XOR
          cgOp _ = error "ICE: Binary operator not implemented"

cgAssign :: SIdentifier -> ModOp -> SExpression -> CodeGenerator [(Maybe Label, MInstruction)]
cgAssign n modop e =
    do (rt, lt, ut) <- loadVariable n
       (re, le, ue) <- cgExpression e
       sequence_ ue >> sequence_ ut
       let load = lt ++ le
           clear = invertInstructions load
       return $ load ++ [(Nothing, cgModOp modop rt re)] ++ clear
    where cgModOp ModAdd = ADD
          cgModOp ModSub = SUB
          cgModOp ModXor = XOR

cgSwap :: SIdentifier -> SIdentifier -> CodeGenerator [(Maybe Label, MInstruction)]
cgSwap n1 n2 = if n1 == n2 then return [] else
    do (r1, l1, u1) <- loadVariable n1
       (r2, l2, u2) <- loadVariable n2
       sequence_ u2 >> sequence_ u1
       let load = l1 ++ l2
           clear = invertInstructions load
       return $ load ++ [(Nothing, XOR r1 r2), (Nothing, XOR r2 r1), (Nothing, XOR r1 r2)] ++ clear

--TODO: Flatten condition evaluation
cgConditional :: SExpression -> [SStatement] -> [SStatement] -> SExpression -> CodeGenerator [(Maybe Label, MInstruction)]
cgConditional e1 s1 s2 e2 =
    do (re1, le1, ue1) <- cgExpression e1
       rt <- tempRegister
       sequence_ ue1
       l_test <- getUniqueLabel "test"
       s1' <- concat <$> mapM cgStatement s1
       l_assert_t <- getUniqueLabel "assert_true"
       l_test_f <- getUniqueLabel "test_false"
       s2' <- concat <$> mapM cgStatement s2
       l_assert <- getUniqueLabel "assert"
       (re2, le2, ue2) <- cgExpression e2
       popTempRegister --rt
       sequence_ ue2
       let test = le1 ++ [(Nothing, XOR rt re1)] ++ invertInstructions le1
           assertion = le2 ++ [(Nothing, XOR rt re2)] ++ invertInstructions le2
           body = [(Just l_test, BEQ rt registerZero l_test_f),
                   (Nothing, XORI rt $ Immediate 1)]
                   ++ s1' ++
                  [(Nothing, XORI rt $ Immediate 1),
                   (Just l_assert_t, BRA l_assert),
                   (Just l_test_f, BRA l_test)]
                   ++ s2' ++
                  [(Just l_assert, BNE rt registerZero l_assert_t)]
       return $ test ++ body ++ assertion

--TODO: Flatten condition/assertion evaluation
cgLoop :: SExpression -> [SStatement] -> [SStatement] -> SExpression -> CodeGenerator [(Maybe Label, MInstruction)]
cgLoop e1 s1 s2 e2 =
    do rt <- tempRegister
       l_entry <- getUniqueLabel "entry"
       (re1, le1, ue1) <- cgExpression e1
       sequence_ ue1
       s1' <- concat <$> mapM cgStatement s1
       (re2, le2, ue2) <- cgExpression e2
       sequence_ ue2
       l_test <- getUniqueLabel "test"
       s2' <- concat <$> mapM cgStatement s2
       l_assert <- getUniqueLabel "assert"
       l_exit <- getUniqueLabel "exit"
       popTempRegister --rt
       let test = le1 ++ [(Nothing, XOR rt re1)] ++ invertInstructions le1
           assertion = le2 ++ [(Nothing, XOR rt re2)] ++ invertInstructions le2
       return $ [(Nothing, XORI rt $ Immediate 1),
                 (Just l_entry, BEQ rt registerZero l_assert)]
                 ++ test ++ s1' ++ assertion ++
                [(Just l_test, BNE rt registerZero l_exit)]
                 ++ s2' ++
                [(Just l_assert, BRA l_entry),
                 (Just l_exit, BRA l_test),
                 (Nothing, XORI rt $ Immediate 1)]

cgObjectBlock :: TypeName -> SIdentifier -> [SStatement] -> CodeGenerator [(Maybe Label, MInstruction)]
cgObjectBlock tp n stmt =
    do rn <- pushRegister n
       rv <- tempRegister
       popTempRegister --rv
       stmt' <- concat <$> mapM cgStatement stmt
       popRegister --rn
       cs <- getClassSize tp
       let l_vtab = "l_" ++ tp ++ "_vt"
           create = [(Nothing, XOR rn registerSP),
                     (Nothing, XORI rv $ AddressMacro l_vtab ),
                     (Nothing, EXCH rv registerSP),
                     (Nothing, ADDI registerSP $ Immediate cs)]
       return $ create ++ stmt' ++ invertInstructions create

cgLocalBlock :: SIdentifier -> SExpression -> [SStatement] -> SExpression -> CodeGenerator [(Maybe Label, MInstruction)]
cgLocalBlock n e1 stmt e2 =
    do rn <- pushRegister n
       (re1, le1, ue1) <- cgExpression e1
       rt1 <- tempRegister
       sequence_ $ popTempRegister : ue1
       stmt' <- concat <$> mapM cgStatement stmt
       (re2, le2, ue2) <- cgExpression e2
       rt2 <- tempRegister
       sequence_ $ popTempRegister : ue2
       popRegister --rn
       let create re rt = [(Nothing, XOR rn registerSP),
                           (Nothing, XOR rt re),
                           (Nothing, EXCH rt registerSP),
                           (Nothing, ADDI registerSP $ Immediate 1)]
           load = le1 ++ create re1 rt1 ++ invertInstructions le1
           clear = le2 ++ invertInstructions (create re2 rt2) ++ invertInstructions le2
       return $ load ++ stmt' ++ clear

loadArgument :: SIdentifier -> CodeGenerator (Register, [(Maybe Label, MInstruction)], [CodeGenerator ()])
loadArgument i = gets (symbolTable . saState) >>= \st ->
    case lookup i st of
        (Just (ClassField _ _ _ o)) ->
            do rt <- tempRegister
               return (rt, [(Nothing, XOR rt registerThis), (Nothing, ADDI rt $ Immediate o)], [popTempRegister])
        _ -> lookupRegister i >>= \r -> return (r, [], [])

cgCall :: [SIdentifier] -> [(Maybe Label, MInstruction)] -> Register -> CodeGenerator [(Maybe Label, MInstruction)]
cgCall args jump this =
    do (ra, la, ua) <- unzip3 <$> mapM loadArgument args
       sequence_ $ concat ua
       rs <- gets registerStack
       let rr = map snd rs \\ (this : ra)
           store = concatMap push $ rr ++ ra ++ [this]
       return $ concat la ++ store ++ jump ++ invertInstructions store ++ invertInstructions (concat la)
    where push r = [(Nothing, EXCH r registerSP), (Nothing, ADDI registerSP $ Immediate 1)]

cgLocalCall :: SIdentifier -> [SIdentifier] -> CodeGenerator [(Maybe Label, MInstruction)]
cgLocalCall m args = getMethodLabel m >>= \l_m -> cgCall args [(Nothing, BRA l_m)] registerThis

cgLocalUncall :: SIdentifier -> [SIdentifier] -> CodeGenerator [(Maybe Label, MInstruction)]
cgLocalUncall m args = getMethodLabel m >>= \l_m -> cgCall args [(Nothing, RBRA l_m)] registerThis

getType :: SIdentifier -> CodeGenerator TypeName
getType i = gets (symbolTable . saState) >>= \st ->
    case lookup i st of
        (Just (LocalVariable (ObjectType tp) _)) -> return tp
        (Just (ClassField (ObjectType tp) _ _ _)) -> return tp
        (Just (MethodParameter (ObjectType tp) _)) -> return tp
        _ -> throwError $ "ICE: Invalid object variable index " ++ show i

loadMethodAddress :: SIdentifier -> MethodName -> CodeGenerator (Register, [(Maybe Label, MInstruction)])
loadMethodAddress o m =
    do ro <- lookupRegister o
       rv <- tempRegister
       rt <- tempRegister
       rtgt <- tempRegister
       popTempRegister >> popTempRegister >> popTempRegister
       offsetMacro <- OffsetMacro <$> getType o <*> pure m
       return (rtgt, [(Nothing, EXCH rv ro), (Nothing, ADDI rv offsetMacro), (Nothing, EXCH rt rv), (Nothing, XOR rtgt rt), (Nothing, EXCH rt rv), (Nothing, SUBI rv offsetMacro), (Nothing, EXCH rv ro)])

cgObjectCall :: SIdentifier -> MethodName -> [SIdentifier] -> CodeGenerator [(Maybe Label, MInstruction)]
cgObjectCall o m args =
    do (rtgt, loadAddress) <- loadMethodAddress o m
       l_jmp <- getUniqueLabel "l_jmp"
       let jp = [(Nothing, SUBI rtgt $ AddressMacro l_jmp),
                 (Just l_jmp, SWAPBR rtgt),
                 (Nothing, NEG rtgt),
                 (Nothing, ADDI rtgt $ AddressMacro l_jmp)]
       call <- (cgCall args jp <=< lookupRegister) o
       return $ loadAddress ++ call ++ invertInstructions loadAddress

cgObjectUncall :: SIdentifier -> MethodName -> [SIdentifier] -> CodeGenerator [(Maybe Label, MInstruction)]
cgObjectUncall o m args =
    do (rtgt, loadAddress) <- loadMethodAddress o m
       l_jmp <- getUniqueLabel "l_jmp"
       l_rjmp_top <- getUniqueLabel "l_rjmp_top"
       l_rjmp_bot <- getUniqueLabel "l_rjmp_bot"
       let jp = [(Nothing, SUBI rtgt $ AddressMacro l_jmp),
                 (Just l_rjmp_top, RBRA l_rjmp_bot),
                 (Just l_jmp, SWAPBR rtgt),
                 (Nothing, NEG rtgt),
                 (Just l_rjmp_bot, BRA l_rjmp_top),
                 (Nothing, ADDI rtgt $ AddressMacro l_jmp)]
       call <- (cgCall args jp <=< lookupRegister) o
       return $ loadAddress ++ call ++ invertInstructions loadAddress

cgStatement :: SStatement -> CodeGenerator [(Maybe Label, MInstruction)]
cgStatement (Assign n modop e) = cgAssign n modop e
cgStatement (Swap n1 n2) = cgSwap n1 n2
cgStatement (Conditional e1 s1 s2 e2) = cgConditional e1 s1 s2 e2
cgStatement (Loop e1 s1 s2 e2) = cgLoop e1 s1 s2 e2
cgStatement (ObjectBlock tp n stmt) = cgObjectBlock tp n stmt
cgStatement (LocalBlock n e1 stmt e2) = cgLocalBlock n e1 stmt e2
cgStatement (LocalCall m args) = cgLocalCall m args
cgStatement (LocalUncall m args) = cgLocalUncall m args
cgStatement (ObjectCall o m args) = cgObjectCall o m args
cgStatement (ObjectUncall o m args) = cgObjectUncall o m args
cgStatement Skip = return []

cgMethod :: (TypeName, SMethodDeclaration) -> CodeGenerator [(Maybe Label, MInstruction)]
cgMethod (_, GMDecl m ps body) =
    do l <- getMethodLabel m
       rs <- addParameters
       body' <- concat <$> mapM cgStatement body
       clearParameters
       let lt = l ++ "_top"
           lb = l ++ "_bot"
           mp = [(Just lt, BRA lb),
                 (Nothing, SUBI registerSP $ Immediate 1),
                 (Nothing, EXCH registerRO registerSP)]
                 ++ concatMap pushParameter rs ++
                [(Nothing, EXCH registerThis registerSP),
                 (Nothing, ADDI registerSP $ Immediate 1),
                 (Just l, SWAPBR registerRO),
                 (Nothing, NEG registerRO),
                 (Nothing, SUBI registerSP $ Immediate 1),
                 (Nothing, EXCH registerThis registerSP)]
                 ++ invertInstructions (concatMap pushParameter rs) ++
                [(Nothing, EXCH registerRO registerSP),
                 (Nothing, ADDI registerSP $ Immediate 1)]
       return $ mp ++ body' ++ [(Just lb, BRA lt)]
    where addParameters = mapM (pushRegister . (\(GDecl _ p) -> p)) ps
          clearParameters = replicateM_ (length ps) popRegister
          pushParameter r = [(Nothing, EXCH r registerSP), (Nothing, ADDI registerSP $ Immediate 1)]

cgVirtualTables :: CodeGenerator [(Maybe Label, MInstruction)]
cgVirtualTables = concat <$> (gets (virtualTables . saState) >>= mapM vtInstructions)
    where vtInstructions (n, ms) = zip (vtLabel n) <$> mapM vtData ms
          vtData m = DATA . AddressMacro <$> getMethodLabel m
          vtLabel n = (Just $ "l_" ++ n ++ "_vt") : repeat Nothing

getMainLabel :: CodeGenerator Label
getMainLabel = gets (mainMethod . saState) >>= getMethodLabel

getMainClass :: CodeGenerator TypeName
getMainClass = gets (mainClass . caState . saState) >>= \mc ->
    case mc of
        (Just tp) -> return tp
        Nothing -> throwError "ICE: No main method defined"

getFields :: TypeName -> CodeGenerator [VariableDeclaration]
getFields tp =
    do cs <- gets (classes . caState . saState)
       case lookup tp cs of
           (Just (GCDecl _ _ fs _)) -> return fs
           Nothing -> throwError $ "ICE: Unknown class " ++ tp

cgOutput :: TypeName -> CodeGenerator ([(Maybe Label, MInstruction)], [(Maybe Label, MInstruction)])
cgOutput tp =
    do mfs <- getFields tp
       co <- concat <$> mapM cgCopyOutput (zip [1..] $ reverse mfs)
       return (map cgStatic mfs, co)
    where cgStatic (GDecl _ n) = (Just $ "l_r_" ++ n, DATA $ Immediate 0)
          cgCopyOutput(o, GDecl _ n) =
              do rt <- tempRegister
                 ra <- tempRegister
                 popTempRegister >> popTempRegister
                 let copy = [SUBI registerSP $ Immediate o,
                             EXCH rt registerSP,
                             XORI ra $ AddressMacro $ "l_r_" ++ n,
                             EXCH rt ra,
                             XORI ra $ AddressMacro $ "l_r_" ++ n,
                             ADDI registerSP $ Immediate o]
                 return $ zip (repeat Nothing) copy

cgProgram :: SProgram -> CodeGenerator PISA.MProgram
cgProgram p =
    do vt <- cgVirtualTables
       rv <- tempRegister
       popTempRegister
       ms <- concat <$> mapM cgMethod p
       l_main <- getMainLabel
       mtp <- getMainClass
       (out, co) <- cgOutput mtp
       let mvt = "l_" ++ mtp ++ "_vt"
           mn = [(Just "start", BRA "top"),
                 (Nothing, START),
                 (Nothing, ADDI registerSP ProgramSize),
                 (Nothing, XOR registerThis registerSP),
                 (Nothing, XORI rv $ AddressMacro mvt),
                 (Nothing, EXCH rv registerSP),
                 (Nothing, ADDI registerSP $ SizeMacro mtp),
                 (Nothing, EXCH registerThis registerSP),
                 (Nothing, ADDI registerSP $ Immediate 1),
                 (Nothing, BRA l_main),
                 (Nothing, SUBI registerSP $ Immediate 1),
                 (Nothing, EXCH registerThis registerSP)]
                 ++ co ++
                [(Nothing, SUBI registerSP $ SizeMacro mtp),
                 (Nothing, EXCH rv registerSP),
                 (Nothing, XORI rv $ AddressMacro mvt),
                 (Nothing, XOR registerThis registerSP),
                 (Nothing, SUBI registerSP ProgramSize),
                 (Just "finish", FINISH)]
       return $ PISA.GProg $ [(Just "top", BRA "start")] ++ out ++ vt ++ ms ++ mn

generatePISA :: (SProgram, SAState) -> Except String (PISA.MProgram, SAState)
generatePISA (p, s) = second saState <$> runStateT (runCG $ cgProgram p) (initialState s)

{--
$0 = 0
$1 = r_sp
$2 = r_ro
$3 = r_this
--}