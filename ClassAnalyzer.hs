{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}

module ClassAnalyzer (classAnalysis, CAState(..)) where

import Data.Maybe
import Data.List

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import AST

data CAState =
    CAState {
        classes :: [(TypeName, ClassDeclaration)],
        superClass :: [(TypeName, Maybe TypeName)],
        subClasses:: [(TypeName, [TypeName])],
        classFields :: [(TypeName, [VariableDeclaration])],
        classMethods :: [(TypeName, [MethodDeclaration])],
        mainClass :: Maybe TypeName
    } deriving (Show, Eq)

newtype ClassAnalyzer a = ClassAnalyzer { runCA :: StateT CAState (Except String) a }
    deriving (Functor, Applicative, Monad, MonadState CAState, MonadError String)

getBaseClass :: TypeName -> ClassAnalyzer (Maybe TypeName)
getBaseClass n = gets superClass >>= \sc ->
    case lookup n sc of
        (Just b) -> return b
        Nothing -> throwError $ "ICE: Unknown class " ++ n

getSubClasses :: TypeName -> ClassAnalyzer [TypeName]
getSubClasses n = gets subClasses >>= \sc ->
    case lookup n sc of
        Nothing -> throwError $ "ICE: Unknown class " ++ n
        (Just sc') -> return sc'

getClassMethods :: TypeName -> ClassAnalyzer [MethodDeclaration]
getClassMethods n = gets classMethods >>= \cm ->
    case lookup n cm of
        Nothing -> throwError $ "ICE: Unknown class " ++ n
        (Just cm') -> return cm'

checkDuplicateClasses :: (TypeName, ClassDeclaration) -> ClassAnalyzer ()
checkDuplicateClasses (n, _) = gets classes >>= \cs ->
    when (count cs > 1) (throwError $ "Multiple definitions of class " ++ n)
    where count = length . filter ((== n) . fst)

checkBaseClass :: (TypeName, Maybe TypeName) -> ClassAnalyzer ()
checkBaseClass (_, Nothing) = return ()
checkBaseClass (n, Just n') =
    do when (n == n') (throwError $ "Class " ++ n ++ " cannot inherit from itself")
       cs <- gets classes
       when (isNothing $ lookup n' cs) (throwError $ "Class " ++ n ++ " cannot inherit from unknown class " ++ n')

checkDuplicateFields :: (TypeName, [VariableDeclaration]) -> ClassAnalyzer ()
checkDuplicateFields (n, fs) = mapM_ checkField fs
    where count v = length . filter (\(GDecl _ v') -> v' == v) $ fs
          checkField (GDecl _ v) = when (count v > 1) (throwError $ "Multiple declarations of field " ++ v ++ " in class " ++ n)

checkDuplicateMethods :: (TypeName, [MethodDeclaration]) -> ClassAnalyzer ()
checkDuplicateMethods (n, ms) = mapM_ checkMethod ms'
    where ms' = map (\(GMDecl n' _ _) -> n') ms
          count m = length . filter (== m) $ ms'
          checkMethod m = when (count m > 1) (throwError $ "Multiple definitions of method " ++ m ++ " in class " ++ n)

checkCyclicInheritance :: (TypeName, Maybe TypeName) -> ClassAnalyzer ()
checkCyclicInheritance (_, Nothing) = return ()
checkCyclicInheritance (n, b) = checkInheritance b [n]
    where checkInheritance Nothing _ = return ()
          checkInheritance (Just b') visited =
              do when (b' `elem` visited) (throwError $ "Cyclic inheritance involving class " ++ n)
                 next <- getBaseClass b'
                 checkInheritance next (b' : visited)

checkBaseMethods :: (TypeName, [MethodDeclaration]) -> ClassAnalyzer ()
checkBaseMethods (n, ms) = getSubClasses n >>= \sc ->
    mapM_ (\m -> mapM_ (checkSubClassMethods m) sc) ms
    where compareMethods (GMDecl m ps _) = all $ \(GMDecl m' ps' _) -> (m /= m') || (m == m' && ps == ps')
          checkSubClassMethods m@(GMDecl mn _ _) sc =
              do cm <- getClassMethods sc
                 unless (compareMethods m cm) (throwError $ "Method " ++ mn ++ " in class " ++ sc ++ " has invalid method signature")
                 sc' <- getSubClasses sc
                 mapM_ (checkSubClassMethods m) sc'

setMainClass :: (TypeName, [MethodDeclaration]) -> ClassAnalyzer ()
setMainClass (n, ms) = when ("main" `elem` ms') (gets mainClass >>= set)
    where ms' = map (\(GMDecl n' _ _) -> n') ms
          set (Just m) = throwError $ "Method main already defined in class " ++ m ++ " but redefined in class " ++ n
          set Nothing = modify $ \s -> s { mainClass = Just n }

checkMainMethod :: ClassAnalyzer ()
checkMainMethod = gets mainClass >>= \mc ->
    when (isNothing mc) (throwError "No main method defined")

initialState :: CAState
initialState = CAState { classes = [], superClass = [], subClasses = [], classFields = [], classMethods = [], mainClass = Nothing }

setSubClasses :: ClassDeclaration -> ClassAnalyzer ()
setSubClasses (GCDecl _ Nothing _ _) = return ()
setSubClasses (GCDecl n (Just b) _ _) = gets subClasses >>= \sc ->
    case lookup b sc of
        Nothing -> throwError $ "ICE: Unknown class " ++ b
        (Just ss) -> modify $ \s -> s { subClasses = (b, n : ss) : delete (b, ss) sc }

caClass :: ClassDeclaration -> ClassAnalyzer ()
caClass c@(GCDecl n b fs ms) = modify $ \s ->
    s { classes = (n, c) : classes s,
        superClass = (n, b) : superClass s,
        subClasses = (n, []) : subClasses s,
        classFields = (n, fs) : classFields s,
        classMethods = (n, ms) : classMethods s }

caProgram :: Program -> ClassAnalyzer Program
caProgram (GProg p) =
    do mapM_ caClass p
       mapM_ setSubClasses p
       s <- get
       mapM_ checkDuplicateClasses $ classes s
       mapM_ checkBaseClass $ superClass s
       mapM_ checkDuplicateFields $ classFields s
       mapM_ checkDuplicateMethods $ classMethods s
       mapM_ checkCyclicInheritance $ superClass s
       mapM_ checkBaseMethods $ classMethods s
       mapM_ setMainClass $ classMethods s
       checkMainMethod
       return $ GProg $ filter noBase p
    where noBase (GCDecl _ Nothing _ _) = True
          noBase _ = False

classAnalysis :: Program -> Either String (Program, CAState)
classAnalysis p = runExcept $ runStateT (runCA $ caProgram p) initialState