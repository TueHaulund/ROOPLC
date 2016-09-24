{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TypeChecker (typeCheck) where

import Data.List

import Control.Monad.Reader
import Control.Monad.Except

import AST
import ClassAnalyzer
import ScopeAnalyzer

newtype TypeChecker a = TypeChecker { runTC :: ReaderT SAState (Except String) a }
    deriving (Functor, Applicative, Monad, MonadReader SAState, MonadError String)

getType :: SIdentifier -> TypeChecker DataType
getType i = asks symbolTable >>= \st ->
    case lookup i st of
        (Just (LocalVariable t _)) -> return t
        (Just (ClassField t _ _)) -> return t
        (Just (MethodParameter t _)) -> return t
        _ -> throwError $ "ICE: Invalid index " ++ show i

getParameterTypes :: SIdentifier -> TypeChecker [DataType]
getParameterTypes i = asks symbolTable >>= \st ->
    case lookup i st of
        (Just (Method ps _)) -> return ps
        _ -> throwError $ "ICE: Invalid index " ++ show i

expectType :: DataType -> DataType -> TypeChecker ()
expectType t1 t2 = unless (t1 == t2) (throwError $ "Expected type: " ++ show t1 ++ "\nActual type: " ++ show t2)

getDynamicParameterTypes :: MethodName -> TypeName -> TypeChecker [DataType]
getDynamicParameterTypes m t = asks (classMethods . caState) >>= \cm ->
    case lookup t cm of
        Nothing -> throwError $ "ICE: Unknown class " ++ t
        (Just ms) -> case find (\(GMDecl m' _ _) -> m == m') ms of
                         Nothing -> throwError $ "Class " ++ t ++ " does not support method " ++ m
                         (Just (GMDecl _ ps _)) -> return $ map (\(GDecl tp _) -> tp) ps

checkCall :: [SIdentifier] -> [DataType] -> TypeChecker ()
checkCall args ps = when (la /= lp) (throwError err) >> mapM_ checkArgument (zip args ps)
    where la = length args
          lp = length ps
          err = "Passed " ++ show la ++ " argument(s) to method expecting " ++ show lp ++ " argument(s)"

checkArgument :: (SIdentifier, DataType) -> TypeChecker ()
checkArgument (i, tp) =
    do ta <- getType i
       sc <- asks $ superClass . caState
       case (ta, tp) of
           (ObjectType ca, ObjectType cp) -> unless (isSubtype sc cp ca) (throwError $ "Class " ++ ca ++ " not a subtype of class " ++ cp)
           _ -> expectType tp ta

isSubtype :: [(TypeName, Maybe TypeName)] -> TypeName -> TypeName -> Bool
isSubtype sc cp ca = (ca == cp) || maybe False (isSubtype sc cp)  (join $ lookup ca sc)

tcExpression :: SExpression -> TypeChecker DataType
tcExpression (Constant _) = pure IntegerType
tcExpression (Variable n) = getType n
tcExpression Nil = pure NilType
tcExpression (Binary binop e1 e2)
    | binop == Eq || binop == Neq =
        do t1 <- tcExpression e1
           t2 <- tcExpression e2
           expectType t1 t2
           pure IntegerType
    | otherwise =
        do t1 <- tcExpression e1
           t2 <- tcExpression e2
           expectType t1 IntegerType
           expectType t2 IntegerType
           pure IntegerType

tcStatement :: SStatement -> TypeChecker ()
tcStatement s =
    case s of
        (Assign n _ e) ->
            getType n
            >>= expectType IntegerType
            >> tcExpression e
            >>= expectType IntegerType

        (Swap n1 n2) ->
            do t1 <- getType n1
               t2 <- getType n2
               expectType t1 t2

        (Conditional e1 s1 s2 e2) ->
            tcExpression e1
            >>= expectType IntegerType
            >> mapM_ tcStatement s1
            >> mapM_ tcStatement s2
            >> tcExpression e2
            >>= expectType IntegerType

        (Loop e1 s1 s2 e2) ->
            tcExpression e1
            >>= expectType IntegerType
            >> mapM_ tcStatement s1
            >> mapM_ tcStatement s2
            >> tcExpression e2
            >>= expectType IntegerType

        (ObjectBlock _ _ stmt) ->
            mapM_ tcStatement stmt

        (LocalBlock n e1 stmt e2) ->
            getType n
            >>= expectType IntegerType
            >> tcExpression e1
            >>= expectType IntegerType
            >> mapM_ tcStatement stmt
            >> tcExpression e2
            >>= expectType IntegerType

        (LocalCall m args) ->
            getParameterTypes m
            >>= checkCall args

        (LocalUncall m args) ->
            getParameterTypes m
            >>= checkCall args

        (ObjectCall o m args) ->
            do t <- getType o
               case t of
                   (ObjectType tn) -> getDynamicParameterTypes m tn >>= checkCall args
                   _ -> throwError $ "Non-object type " ++ show t ++ " does not support method invocation"

        (ObjectUncall o m args) ->
            do t <- getType o
               case t of
                   (ObjectType tn) -> getDynamicParameterTypes m tn >>= checkCall args
                   _ -> throwError $ "Non-object type " ++ show t ++ " does not support method invocation"

        Skip -> pure ()

getMethodName :: SIdentifier -> TypeChecker Identifier
getMethodName i = asks symbolTable >>= \st ->
    case lookup i st of
        (Just (Method _ n)) -> return n
        _ -> throwError $ "ICE: Invalid index " ++ show i

tcMethod :: (TypeName, SMethodDeclaration) -> TypeChecker ()
tcMethod (_, GMDecl _ [] body) = mapM_ tcStatement body
tcMethod (_, GMDecl i (_:_) body) = getMethodName i >>= \n ->
    when (n == "main") (throwError "Method main has invalid signature")
    >> mapM_ tcStatement body

tcProgram :: SProgram -> TypeChecker ()
tcProgram = mapM_ tcMethod

typeCheck :: (SProgram, SAState) -> Either String ()
typeCheck (p, s) = runExcept $ runReaderT (runTC $ tcProgram p) s