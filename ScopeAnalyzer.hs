{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ScopeAnalyzer (scopeAnalysis, SAState(..)) where

import Data.Maybe
import Data.List

import Control.Monad.State
import Control.Monad.Except

import AST
import ClassAnalyzer

data SAState =
    SAState {
        variableIndex :: SIdentifier,
        symbolTable :: SymbolTable,
        scopeStack :: [Scope],
        caState :: CAState
    } deriving (Show, Eq)

newtype ScopeAnalyzer a = ScopeAnalyzer { runSA :: StateT SAState (Except String) a }
    deriving (Functor, Applicative, Monad, MonadState SAState, MonadError String)

initialState :: CAState -> SAState
initialState s = SAState { variableIndex = 0, symbolTable = [], scopeStack = [], caState = s }

enterScope :: ScopeAnalyzer ()
enterScope = modify $ \s -> s { scopeStack = [] : scopeStack s }

leaveScope :: ScopeAnalyzer ()
leaveScope = modify $ \s -> s { scopeStack = drop 1 $ scopeStack s }

topScope :: ScopeAnalyzer Scope
topScope = gets scopeStack >>= \ss ->
    case ss of
        (s:_) -> return s
        [] -> throwError "ICE: Empty scope stack"

addToScope :: (Identifier, SIdentifier) -> ScopeAnalyzer ()
addToScope b = do ts <- topScope
                  modify $ \s -> s { scopeStack = (b : ts) : scopeStack s }

registerSymbol :: VariableType -> Identifier -> ScopeAnalyzer SIdentifier
registerSymbol vt n =
    do ts <- topScope
       when (isJust $ lookup n ts) (throwError $ "Redeclaration of variable: " ++ n)
       i <- gets variableIndex
       modify $ \s -> s { symbolTable = (i, vt) : symbolTable s, variableIndex = 1 + i }
       addToScope (n, i)
       return i

saLookup :: Identifier -> ScopeAnalyzer SIdentifier
saLookup n = gets scopeStack >>= \ss ->
    case listToMaybe $ mapMaybe (lookup n) ss of
        Nothing -> throwError $ "Undeclared variable: " ++ n
        Just i -> return i

getBaseClass :: TypeName -> ScopeAnalyzer (Maybe TypeName)
getBaseClass t = do bs <- gets $ inherits . caState
                    case lookup t bs of
                        Nothing -> throwError $ "ICE: Unknown class " ++ t
                        (Just b) -> return b

getOwnFields :: TypeName -> ScopeAnalyzer [VariableDeclaration]
getOwnFields t = do fs <- gets $ fields . caState
                    case lookup t fs of
                        Nothing -> throwError $ "ICE: Unknown class " ++ t
                        (Just f) -> return f

getClassFields :: TypeName -> ScopeAnalyzer [VariableDeclaration]
getClassFields t = do bs <- getBaseClass t
                      fs <- getOwnFields t
                      case bs of
                          Nothing -> return fs
                          (Just b) -> getClassFields b >>= inheritedFields fs
    where inheritedFields fs fs' = return $ unionBy compareFields fs fs'
          compareFields (GDecl _ n1) (GDecl _ n2) = n1 == n2

saExpression :: Expression -> ScopeAnalyzer SExpression
saExpression (Constant v) = pure $ Constant v
saExpression (Variable n) = Variable <$> saLookup n
saExpression Nil = pure Nil
saExpression (Binary binop e1 e2) =
    Binary binop
    <$> saExpression e1
    <*> saExpression e2

saStatement :: Statement -> ScopeAnalyzer SStatement
saStatement s =
    case s of
        (Assign n modop e) ->
            when (elem n $ var e) (throwError "Irreversible variable assignment")
            >> Assign
            <$> saLookup n
            <*> pure modop
            <*> saExpression e

        (Swap n1 n2) ->
            Swap
            <$> saLookup n1
            <*> saLookup n2

        (Conditional e1 s1 s2 e2) ->
            Conditional
            <$> saExpression e1
            <*> mapM saStatement s1
            <*> mapM saStatement s2
            <*> saExpression e2

        (Loop e1 s1 s2 e2) ->
            Loop
            <$> saExpression e1
            <*> mapM saStatement s1
            <*> mapM saStatement s2
            <*> saExpression e2

        (ObjectBlock tp n stmt) ->
            do enterScope
               n' <- registerSymbol (LocalVariable (ObjectType tp) n) n
               stmt' <- mapM saStatement stmt
               leaveScope
               return $ ObjectBlock tp n' stmt'

        (LocalBlock n e1 stmt e2) ->
            do e1' <- saExpression e1
               enterScope
               n' <- registerSymbol (LocalVariable IntegerType n) n
               stmt' <- mapM saStatement stmt
               leaveScope
               e2' <- saExpression e2
               return $ LocalBlock n' e1' stmt' e2'

        (LocalCall m args) ->
            LocalCall
            <$> pure m
            <*> localCall m args

        (LocalUncall m args) ->
            LocalUncall
            <$> pure m
            <*> localCall m args

        (ObjectCall o m args) ->
            when (args /= nub args || o `elem` args) (throwError $ "Irreversible invocation of method " ++ m)
            >> ObjectCall
            <$> saLookup o
            <*> pure m
            <*> mapM saLookup args

        (ObjectUncall o m args) ->
            when (args /= nub args || o `elem` args) (throwError $ "Irreversible invocation of method " ++ m)
            >> ObjectUncall
            <$> saLookup o
            <*> pure m
            <*> mapM saLookup args

        Skip -> pure Skip

    where var (Variable n) = [n]
          var (Binary _ e1 e2) = var e1 ++ var e2
          var _ = []

          isCF ClassField{} = True
          isCF _ = False

          rlookup = flip lookup

          localCall :: MethodName -> [Identifier] -> ScopeAnalyzer [SIdentifier]
          localCall m args =
            do when (args /= nub args) (throwError $ "Irreversible invocation of method " ++ m)
               args' <- mapM saLookup args
               st <- gets symbolTable
               when (any isCF $ mapMaybe (rlookup st) args') (throwError $ "Irreversible invocation of method " ++ m)
               return args'

saMethod :: (TypeName, MethodDeclaration) -> ScopeAnalyzer (TypeName, SMethodDeclaration)
saMethod (t, GMDecl m ps body) =
    do enterScope
       fs <- getClassFields t
       mapM_ registerCF fs
       enterScope
       ps' <- mapM registerMP ps
       body' <- mapM saStatement body
       leaveScope
       leaveScope
       return (t, GMDecl m ps' body')
    where registerCF (GDecl tp n) = registerSymbol (ClassField tp n t) n
          registerMP (GDecl tp n) = GDecl tp <$> registerSymbol (MethodParameter tp n) n

saProgram :: PProgram -> ScopeAnalyzer SProgram
saProgram = mapM saMethod

scopeAnalysis :: (PProgram, CAState) -> Either String (SProgram, SAState)
scopeAnalysis (p, s) = runExcept $ runStateT (runSA $ saProgram p) $ initialState s