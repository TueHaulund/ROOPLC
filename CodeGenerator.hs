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

{-# ANN module "HLint: ignore Reduce duplication" #-}

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

loadVariableAddress :: SIdentifier -> CodeGenerator (Register, [(Maybe Label, MInstruction)], CodeGenerator ())
loadVariableAddress n = gets (symbolTable . saState) >>= \st ->
    case lookup n st of
        (Just (ClassField _ _ _ o)) -> tempRegister >>= \r -> return (r, [(Nothing, ADD r registerThis), (Nothing, ADDI r $ Immediate o)], popTempRegister)
        (Just (LocalVariable _ _)) -> lookupRegister n >>= \r -> return (r, [], return ())
        (Just (MethodParameter _ _)) -> lookupRegister n >>= \r -> return (r, [], return ())
        _ -> throwError $ "ICE: Invalid variable index " ++ show n

loadVariableValue :: SIdentifier -> CodeGenerator (Register, [(Maybe Label, MInstruction)], CodeGenerator ())
loadVariableValue n =
    do (ra, la, ua) <- loadVariableAddress n
       rv <- tempRegister
       return (rv, la ++ [(Nothing, EXCH rv ra)], popTempRegister >> ua)

cgBinOp :: BinOp -> Register -> Register -> CodeGenerator (Register, [(Maybe Label, MInstruction)], CodeGenerator ())
cgBinOp Add r1 r2 = tempRegister >>= \rt -> return (rt, [(Nothing, XOR rt r1), (Nothing, ADD rt r2)], popTempRegister)
cgBinOp Sub r1 r2 = tempRegister >>= \rt -> return (rt, [(Nothing, XOR rt r1), (Nothing, SUB rt r2)], popTempRegister)
cgBinOp Xor r1 r2 = tempRegister >>= \rt -> return (rt, [(Nothing, XOR rt r1), (Nothing, XOR rt r2)], popTempRegister)
cgBinOp BitAnd r1 r2 = tempRegister >>= \rt -> return (rt, [(Nothing, ANDX rt r1 r2)], popTempRegister)
cgBinOp BitOr r1 r2 = tempRegister >>= \rt -> return (rt, [(Nothing, ORX rt r1 r2)], popTempRegister)
cgBinOp Lt r1 r2 =
    do rt <- tempRegister
       rc <- tempRegister
       l_top <- getUniqueLabel "cmp_top"
       l_bot <- getUniqueLabel "cmp_bot"
       let cmp = [(Nothing, XOR rt r1),
                  (Nothing, SUB rt r2),
                  (Just l_top, BGEZ rt l_bot),
                  (Nothing, XORI rc $ Immediate 1),
                  (Just l_bot, BGEZ rt l_top)]
       return (rc, cmp, popTempRegister >> popTempRegister)
cgBinOp Gt r1 r2 =
    do rt <- tempRegister
       rc <- tempRegister
       l_top <- getUniqueLabel "cmp_top"
       l_bot <- getUniqueLabel "cmp_bot"
       let cmp = [(Nothing, XOR rt r1),
                  (Nothing, SUB rt r2),
                  (Just l_top, BLEZ rt l_bot),
                  (Nothing, XORI rc $ Immediate 1),
                  (Just l_bot, BLEZ rt l_top)]
       return (rc, cmp, popTempRegister >> popTempRegister)
cgBinOp Eq r1 r2 =
    do rt <- tempRegister
       l_top <- getUniqueLabel "cmp_top"
       l_bot <- getUniqueLabel "cmp_bot"
       let cmp = [(Just l_top, BNE r1 r2 l_bot),
                  (Nothing, XORI rt $ Immediate 1),
                  (Just l_bot, BNE r1 r2 l_top)]
       return (rt, cmp, popTempRegister)
cgBinOp Neq r1 r2 =
    do rt <- tempRegister
       l_top <- getUniqueLabel "cmp_top"
       l_bot <- getUniqueLabel "cmp_bot"
       let cmp = [(Just l_top, BEQ r1 r2 l_bot),
                  (Nothing, XORI rt $ Immediate 1),
                  (Just l_bot, BEQ r1 r2 l_top)]
       return (rt, cmp, popTempRegister)
cgBinOp Lte r1 r2 =
    do rt <- tempRegister
       rc <- tempRegister
       l_top <- getUniqueLabel "cmp_top"
       l_bot <- getUniqueLabel "cmp_bot"
       let cmp = [(Nothing, XOR rt r1),
                  (Nothing, SUB rt r2),
                  (Just l_top, BGTZ rt l_bot),
                  (Nothing, XORI rc $ Immediate 1),
                  (Just l_bot, BGTZ rt l_top)]
       return (rc, cmp, popTempRegister >> popTempRegister)
cgBinOp Gte r1 r2 =
    do rt <- tempRegister
       rc <- tempRegister
       l_top <- getUniqueLabel "cmp_top"
       l_bot <- getUniqueLabel "cmp_bot"
       let cmp = [(Nothing, XOR rt r1),
                  (Nothing, SUB rt r2),
                  (Just l_top, BLTZ rt l_bot),
                  (Nothing, XORI rc $ Immediate 1),
                  (Just l_bot, BLTZ rt l_top)]
       return (rc, cmp, popTempRegister >> popTempRegister)
cgBinOp _ _ _ = throwError "ICE: Binary operator not implemented"

cgExpression :: SExpression -> CodeGenerator (Register, [(Maybe Label, MInstruction)], CodeGenerator ())
cgExpression (Constant 0) = return (registerZero, [], return ())
cgExpression (Constant n) = tempRegister >>= \rt -> return (rt, [(Nothing, XORI rt $ Immediate n)], popTempRegister)
cgExpression (Variable i) = loadVariableValue i
cgExpression Nil = return (registerZero, [], return ())
cgExpression (Binary op e1 e2) =
    do (r1, l1, u1) <- cgExpression e1
       (r2, l2, u2) <- cgExpression e2
       (ro, lo, uo) <- cgBinOp op r1 r2
       return (ro, l1 ++ l2 ++ lo, uo >> u2 >> u1)

cgBinaryExpression :: SExpression -> CodeGenerator (Register, [(Maybe Label, MInstruction)], CodeGenerator ())
cgBinaryExpression e =
    do (re, le, ue) <- cgExpression e
       rt <- tempRegister
       l_top <- getUniqueLabel "f_top"
       l_bot <- getUniqueLabel "f_bot"
       let flatten = [(Just l_top, BEQ re registerZero l_bot),
                      (Nothing, XORI rt $ Immediate 1),
                      (Just l_bot, BEQ re registerZero l_top)]
       return (rt, le ++ flatten, popTempRegister >> ue)

cgAssign :: SIdentifier -> ModOp -> SExpression -> CodeGenerator [(Maybe Label, MInstruction)]
cgAssign n modop e =
    do (rt, lt, ut) <- loadVariableValue n
       (re, le, ue) <- cgExpression e
       ue >> ut
       return $ lt ++ le ++ [(Nothing, cgModOp modop rt re)] ++ invertInstructions (lt ++ le)
    where cgModOp ModAdd = ADD
          cgModOp ModSub = SUB
          cgModOp ModXor = XOR

loadForSwap :: SIdentifier -> CodeGenerator (Register, [(Maybe Label, MInstruction)], CodeGenerator ())
loadForSwap n = gets (symbolTable . saState) >>= \st ->
    case lookup n st of
        (Just ClassField {}) -> loadVariableValue n
        (Just (LocalVariable IntegerType _)) -> loadVariableValue n
        (Just (LocalVariable (ObjectType _) _)) -> loadVariableAddress n
        (Just (MethodParameter IntegerType _)) -> loadVariableValue n
        (Just (MethodParameter (ObjectType _) _)) -> loadVariableAddress n
        _ -> throwError $ "ICE: Invalid variable index " ++ show n

cgSwap :: SIdentifier -> SIdentifier -> CodeGenerator [(Maybe Label, MInstruction)]
cgSwap n1 n2 = if n1 == n2 then return [] else
    do (r1, l1, u1) <- loadForSwap n1
       (r2, l2, u2) <- loadForSwap n2
       u2 >> u1
       let swap = [(Nothing, XOR r1 r2), (Nothing, XOR r2 r1), (Nothing, XOR r1 r2)]
       return $ l1 ++ l2 ++ swap ++ invertInstructions (l1 ++ l2)

cgConditional :: SExpression -> [SStatement] -> [SStatement] -> SExpression -> CodeGenerator [(Maybe Label, MInstruction)]
cgConditional e1 s1 s2 e2 =
    do l_test <- getUniqueLabel "test"
       l_assert_t <- getUniqueLabel "assert_true"
       l_test_f <- getUniqueLabel "test_false"
       l_assert <- getUniqueLabel "assert"
       rt <- tempRegister
       (re1, le1, ue1) <- cgBinaryExpression e1
       ue1
       s1' <- concat <$> mapM cgStatement s1
       s2' <- concat <$> mapM cgStatement s2
       (re2, le2, ue2) <- cgBinaryExpression e2
       ue2 >> popTempRegister --rt
       return $ le1 ++ [(Nothing, XOR rt re1)] ++ invertInstructions le1 ++
                [(Just l_test, BEQ rt registerZero l_test_f), (Nothing, XORI rt $ Immediate 1)] ++
                s1' ++ [(Nothing, XORI rt $ Immediate 1), (Just l_assert_t, BRA l_assert), (Just l_test_f, BRA l_test)] ++
                s2' ++ [(Just l_assert, BNE rt registerZero l_assert_t)] ++
                le2 ++ [(Nothing, XOR rt re2)] ++ invertInstructions le2

cgLoop :: SExpression -> [SStatement] -> [SStatement] -> SExpression -> CodeGenerator [(Maybe Label, MInstruction)]
cgLoop e1 s1 s2 e2 =
    do l_entry <- getUniqueLabel "entry"
       l_test <- getUniqueLabel "test"
       l_assert <- getUniqueLabel "assert"
       l_exit <- getUniqueLabel "exit"
       rt <- tempRegister
       (re1, le1, ue1) <- cgBinaryExpression e1
       ue1
       s1' <- concat <$> mapM cgStatement s1
       s2' <- concat <$> mapM cgStatement s2
       (re2, le2, ue2) <- cgBinaryExpression e2
       ue2 >> popTempRegister --rt
       return $ [(Nothing, XORI rt $ Immediate 1), (Just l_entry, BEQ rt registerZero l_assert)] ++
                le1 ++ [(Nothing, XOR rt re1)] ++ invertInstructions le1 ++
                s1' ++ le2 ++ [(Nothing, XOR rt re2)] ++ invertInstructions le2 ++
                [(Just l_test, BNE rt registerZero l_exit)] ++ s2' ++
                [(Just l_assert, BRA l_entry), (Just l_exit, BRA l_test), (Nothing, XORI rt $ Immediate 1)]

cgObjectBlock :: TypeName -> SIdentifier -> [SStatement] -> CodeGenerator [(Maybe Label, MInstruction)]
cgObjectBlock tp n stmt =
    do rn <- pushRegister n
       rv <- tempRegister
       popTempRegister --rv
       stmt' <- concat <$> mapM cgStatement stmt
       popRegister --rn
       cs <- getClassSize tp
       let create = [(Nothing, XOR rn registerSP),
                     (Nothing, XORI rv $ AddressMacro $ "l_" ++ tp ++ "_vt"),
                     (Nothing, EXCH rv registerSP),
                     (Nothing, ADDI registerSP $ Immediate cs)]
       return $ create ++ stmt' ++ invertInstructions create

cgLocalBlock :: SIdentifier -> SExpression -> [SStatement] -> SExpression -> CodeGenerator [(Maybe Label, MInstruction)]
cgLocalBlock n e1 stmt e2 =
    do rn <- pushRegister n
       (re1, le1, ue1) <- cgExpression e1
       rt1 <- tempRegister
       popTempRegister >> ue1
       stmt' <- concat <$> mapM cgStatement stmt
       (re2, le2, ue2) <- cgExpression e2
       rt2 <- tempRegister
       popTempRegister >> ue2
       popRegister --rn
       let create re rt = [(Nothing, XOR rn registerSP),
                           (Nothing, XOR rt re),
                           (Nothing, EXCH rt registerSP),
                           (Nothing, ADDI registerSP $ Immediate 1)]
           load = le1 ++ create re1 rt1 ++ invertInstructions le1
           clear = le2 ++ invertInstructions (create re2 rt2) ++ invertInstructions le2
       return $ load ++ stmt' ++ clear

cgCall :: [SIdentifier] -> [(Maybe Label, MInstruction)] -> Register -> CodeGenerator [(Maybe Label, MInstruction)]
cgCall args jump this =
    do (ra, la, ua) <- unzip3 <$> mapM loadVariableAddress args
       sequence_ ua
       rs <- gets registerStack
       let rr = (registerThis : map snd rs) \\ (this : ra)
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

loadMethodAddress :: (SIdentifier, Register) -> MethodName -> CodeGenerator (Register, [(Maybe Label, MInstruction)])
loadMethodAddress (o, ro) m =
    do rv <- tempRegister
       rt <- tempRegister
       rtgt <- tempRegister
       popTempRegister >> popTempRegister >> popTempRegister
       offsetMacro <- OffsetMacro <$> getType o <*> pure m
       let load = [(Nothing, EXCH rv ro),
                   (Nothing, ADDI rv offsetMacro),
                   (Nothing, EXCH rt rv),
                   (Nothing, XOR rtgt rt),
                   (Nothing, EXCH rt rv),
                   (Nothing, SUBI rv offsetMacro),
                   (Nothing, EXCH rv ro)]
       return (rtgt, load)

loadForCall :: SIdentifier -> CodeGenerator (Register, [(Maybe Label, MInstruction)], CodeGenerator ())
loadForCall n = gets (symbolTable . saState) >>= \st ->
    case lookup n st of
        (Just ClassField {}) -> loadVariableValue n
        (Just _) -> loadVariableAddress n
        _ -> throwError $ "ICE: Invalid variable index " ++ show n

cgObjectCall :: SIdentifier -> MethodName -> [SIdentifier] -> CodeGenerator [(Maybe Label, MInstruction)]
cgObjectCall o m args =
    do (ro, lo, uo) <- loadForCall o
       rt <- tempRegister
       (rtgt, loadAddress) <- loadMethodAddress (o, rt) m
       l_jmp <- getUniqueLabel "l_jmp"
       let jp = [(Nothing, SUBI rtgt $ AddressMacro l_jmp),
                 (Just l_jmp, SWAPBR rtgt),
                 (Nothing, NEG rtgt),
                 (Nothing, ADDI rtgt $ AddressMacro l_jmp)]
       call <- cgCall args jp rt
       popTempRegister >> uo
       let load = lo ++ [(Nothing, XOR rt ro)] ++ loadAddress ++ invertInstructions lo
       return $ load ++ call ++ invertInstructions load

cgObjectUncall :: SIdentifier -> MethodName -> [SIdentifier] -> CodeGenerator [(Maybe Label, MInstruction)]
cgObjectUncall o m args =
    do (ro, lo, uo) <- loadForCall o
       rt <- tempRegister
       (rtgt, loadAddress) <- loadMethodAddress (o, rt) m
       l_jmp <- getUniqueLabel "l_jmp"
       l_rjmp_top <- getUniqueLabel "l_rjmp_top"
       l_rjmp_bot <- getUniqueLabel "l_rjmp_bot"
       let jp = [(Nothing, SUBI rtgt $ AddressMacro l_jmp),
                 (Just l_rjmp_top, RBRA l_rjmp_bot),
                 (Just l_jmp, SWAPBR rtgt),
                 (Nothing, NEG rtgt),
                 (Just l_rjmp_bot, BRA l_rjmp_top),
                 (Nothing, ADDI rtgt $ AddressMacro l_jmp)]
       call <- cgCall args jp rt
       popTempRegister >> uo
       let load = lo ++ [(Nothing, XOR rt ro)] ++ loadAddress ++ invertInstructions lo
       return $ load ++ call ++ invertInstructions load

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