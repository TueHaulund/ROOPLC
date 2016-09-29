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
lookupRegister i =
    do rs <- gets registerStack
       st <- gets (symbolTable . saState)
       case lookup i rs of
           Nothing -> throwError $ "ICE: No register reserved for index " ++ show i ++ (show $ lookup i st)
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

loadVariable :: SIdentifier -> CodeGenerator (Register, [(Maybe Label, MInstruction)])
loadVariable i = gets (symbolTable . saState) >>= \st ->
    case lookup i st of
        (Just (LocalVariable _ _)) -> dereference
        (Just (MethodParameter _ _)) -> dereference
        (Just (ClassField _ n t o)) -> do ro <- tempRegister
                                          rf <- tempRegister
                                          return (rf, [(Nothing, ADD ro registerThis), (Nothing, ADDI ro $ Immediate o), (Nothing, EXCH rf ro)])
        _ -> throwError $ "ICE: Invalid variable index " ++ show i
    where dereference = do r <- lookupRegister i
                           rt <- tempRegister
                           return (rt, [(Nothing, EXCH rt r)])

clearVariable :: (SIdentifier, [(Maybe Label, MInstruction)]) -> CodeGenerator [(Maybe Label, MInstruction)]
clearVariable (i, p) = gets (symbolTable . saState) >>= \st ->
    case lookup i st of
        (Just (LocalVariable _ _)) -> popTempRegister >> return p'
        (Just (MethodParameter _ _)) -> popTempRegister >> return p'
        (Just (ClassField _ _ _ _)) -> popTempRegister >> popTempRegister >> return p'
        _ -> throwError $ "ICE: Invalid variable index " ++ show i
    where p' = invertInstructions p

cgExpression :: SExpression -> CodeGenerator (Register, [(Maybe Label, MInstruction)])
cgExpression (Constant n) =
    do rt <- tempRegister
       return (rt, [(Nothing, XORI rt $ Immediate n)])
cgExpression (Variable i) = loadVariable i
cgExpression Nil = return (registerZero, [])
cgExpression (Binary op e1 e2) =
    do (r1, l1) <- cgExpression e1
       (r2, l2) <- cgExpression e2
       rt <- tempRegister
       case op of
           Add -> return $ (rt, l1 ++ l2 ++ [(Nothing, XOR rt r1), (Nothing, ADD rt r2)])
           Sub -> return $ (rt, l1 ++ l2 ++ [(Nothing, XOR rt r1), (Nothing, SUB rt r2)])
           Xor -> return $ (rt, l1 ++ l2 ++ [(Nothing, XOR rt r1), (Nothing, XOR rt r2)])
           _ -> throwError $ "ICE: Binary operator not implemented"

clearExpression :: (SExpression, [(Maybe Label, MInstruction)]) -> CodeGenerator [(Maybe Label, MInstruction)]
clearExpression (e, p) =
    case e of
        (Constant _) -> popTempRegister >> return p'
        (Variable i) -> clearVariable (i, p)
        Nil -> return p'
        (Binary op e1 e2) -> popTempRegister >> clearExpression (e2, p) >> clearExpression (e1, p) >> return p'
    where p' = invertInstructions p

cgModOp :: ModOp -> (Register -> Register -> MInstruction)
cgModOp ModAdd = ADD
cgModOp ModSub = SUB
cgModOp ModXor = XOR

cgStatement :: SStatement -> CodeGenerator [(Maybe Label, MInstruction)]
cgStatement s =
    case s of
        (Assign n modop e) ->
            do (rt, lt) <- loadVariable n
               (re, le) <- cgExpression e
               ce <- clearExpression (e, le)
               ct <- clearVariable (n, lt)
               return $ lt ++ le ++ [(Nothing, cgModOp modop rt re)] ++ ce ++ ct

        (Swap n1 n2) ->
            if n1 == n2 then return [] else
                do (r1, l1) <- loadVariable n1
                   (r2, l2) <- loadVariable n2
                   c2 <- clearVariable (n2, l2)
                   c1 <- clearVariable (n1, l1)
                   return $ l1 ++ l2 ++ [(Nothing, XOR r1 r2), (Nothing, XOR r2 r1), (Nothing, XOR r1 r2)] ++ c2 ++ c1

        --TODO: Flatten condition evaluation
        (Conditional e1 s1 s2 e2) ->
            do (re1, le1) <- cgExpression e1
               rt <- tempRegister
               ce1 <- clearExpression (e1, le1)
               l_test <- getUniqueLabel "test"
               s1' <- concat <$> mapM cgStatement s1
               l_assert_t <- getUniqueLabel "assert_true"
               l_test_f <- getUniqueLabel "test_false"
               s2' <- concat <$> mapM cgStatement s2
               l_assert <- getUniqueLabel "assert"
               (re2, le2) <- cgExpression e2
               popTempRegister --pop rt
               ce2 <- clearExpression (e2, le2)
               return $ le1 ++ [(Nothing, XOR rt re1)] ++ ce1 ++ [(Just l_test, BEQ rt registerZero l_test_f), (Nothing, XORI rt $ Immediate 1)] ++ s1' ++ [(Nothing, XORI rt $ Immediate 1), (Just l_assert_t, BRA l_assert), (Just l_test_f, BRA l_test)] ++ s2' ++ [(Just l_assert, BNE rt registerZero l_assert_t)] ++ le2 ++ [(Nothing, XOR rt re2)] ++ ce2

        --TODO: Flatten condition/assertion evaluation
        (Loop e1 s1 s2 e2) ->
            do rt <- tempRegister
               l_entry <- getUniqueLabel "entry"
               (re1, le1) <- cgExpression e1
               ce1 <- clearExpression (e1, le1)
               s1' <- concat <$> mapM cgStatement s1
               (re2, le2) <- cgExpression e2
               ce2 <- clearExpression (e2, le2)
               l_test <- getUniqueLabel "test"
               s2' <- concat <$> mapM cgStatement s2
               l_assert <- getUniqueLabel "assert"
               l_exit <- getUniqueLabel "exit"
               popTempRegister
               return $ [(Nothing, XORI rt $ Immediate 1), (Just l_entry, BEQ rt registerZero l_assert)] ++ le1 ++ [(Nothing, XOR rt re1)] ++ ce1 ++ s1' ++ le2 ++ [(Nothing, XOR rt re2)] ++ ce2 ++ [(Just l_test, BNE rt registerZero l_exit)] ++ s2' ++ [(Just l_assert, BRA l_entry), (Just l_exit, BRA l_test), (Nothing, XORI rt $ Immediate 1)]

        (ObjectBlock tp n stmt) ->
            do rn <- pushRegister n
               rv <- tempRegister
               popRegister
               stmt' <- concat <$> mapM cgStatement stmt
               popRegister
               let create = [(Nothing, XOR rn registerSP), (Nothing, XORI rv $ Immediate 0), (Nothing, EXCH rv registerSP), (Nothing, ADDI registerSP $ Immediate 0)] --TODO
               return $ create ++ stmt'++ (invertInstructions create)

        (LocalBlock n e1 stmt e2) ->
            do rn <- pushRegister n
               --rn <- e1
               (re1, le1) <- cgExpression e1
               rt1 <- tempRegister
               popTempRegister
               ce1 <- clearExpression (e1, le1)
               stmt' <- concat <$> mapM cgStatement stmt
               --rn -> e2
               (re2, le2) <- cgExpression e2
               rt2 <- tempRegister
               popTempRegister
               ce2 <- clearExpression (e2, le2)
               popRegister
               let create re rt = [(Nothing, XOR rn registerSP), (Nothing, XOR rt re), (Nothing, EXCH rt registerSP), (Nothing, ADDI registerSP $ Immediate 1)]
               return $ le1 ++ (create re1 rt1) ++ ce1 ++ stmt' ++ le2 ++ (invertInstructions $ create re2 rt2) ++ ce2

        (LocalCall m args) -> return []

        (LocalUncall m args) -> return []

        (ObjectCall o m args) -> return []

        (ObjectUncall o m args) -> return []

        Skip -> return []

cgMethod :: (TypeName, SMethodDeclaration) -> CodeGenerator [(Maybe Label, MInstruction)]
cgMethod (n, GMDecl m ps body) =
    do l <- getMethodLabel m
       rs <- addParameters
       body' <- concat <$> mapM cgStatement body
       clearParameters
       let lt = l ++ "_top"
           lb = l ++ "_bot"
           mp = [(Just lt, BRA lb),
                 (Nothing, ADDI registerSP $ Immediate (-1)),
                 (Nothing, EXCH registerRO registerSP)]
                 ++ (concat $ map pushParameter rs) ++
                [(Nothing, EXCH registerThis registerSP),
                 (Nothing, ADDI registerSP $ Immediate 1),
                 (Just l, SWAPBR registerRO),
                 (Nothing, NEG registerRO),
                 (Nothing, ADDI registerSP $ Immediate (-1)),
                 (Nothing, EXCH registerThis registerSP)]
                 ++ (concat $ map popParameter rs) ++
                [(Nothing, EXCH registerRO registerSP),
                 (Nothing, ADDI registerSP $ Immediate 1)]
       return $ mp ++ body' ++ [(Just lb, BRA lt)]
    where addParameters = mapM pushRegister $ map (\(GDecl _ p) -> p) ps
          clearParameters = replicateM_ (length ps) popRegister
          popParameter r = [(Nothing, ADDI registerSP $ Immediate (-1)), (Nothing, EXCH r registerSP)]
          pushParameter r = [(Nothing, EXCH r registerSP), (Nothing, ADDI registerSP $ Immediate 1)]

cgProgram :: SProgram -> CodeGenerator PISA.MProgram
cgProgram p = PISA.GProg <$> concat <$> mapM cgMethod p

generatePISA :: (SProgram, SAState) -> Except String (PISA.MProgram, CAState)
generatePISA (p, s) = second (caState . saState) <$> (runStateT (runCG $ cgProgram p) $ initialState s)

{--
$0 = 0
$1 = r_sp
$2 = r_ro
$3 = r_this
--}