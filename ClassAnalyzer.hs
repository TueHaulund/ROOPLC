{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

checkDuplicateClasses :: (TypeName, ClassDeclaration) -> ClassAnalyzer ()
checkDuplicateClasses (n, _) =
    do cs <- gets classes
       when (count cs > 1) (throwError $ "Multiple definitions of class " ++ n)
    where count = length . filter ((== n) . fst)

checkBaseClass :: (TypeName, Maybe TypeName) -> ClassAnalyzer ()
checkBaseClass (_, Nothing) = return ()
checkBaseClass (n, Just n') =
    do when (n == n') (throwError $ "Class " ++ n ++ " cannot inherit from itself")
       cs <- gets classes
       when (isNothing $ lookup n' cs) (throwError $ "Class " ++ n ++ " cannot inherit from unknown class " ++ n')

checkDuplicateFields :: (TypeName, [VariableDeclaration]) -> ClassAnalyzer ()
checkDuplicateFields (n, fs) = mapM_ (checkField n fs) fs

checkField :: TypeName -> [VariableDeclaration] -> VariableDeclaration -> ClassAnalyzer ()
checkField n fs (GDecl _ v) = when (count > 1) (throwError $ "Multiple declarations of field " ++ v ++ " in class " ++ n)
    where count = length . filter (\(GDecl _ v') -> v' == v) $ fs

checkDuplicateMethods :: (TypeName, [MethodDeclaration]) -> ClassAnalyzer ()
checkDuplicateMethods (n, ms) = mapM_ (checkMethod n ms') ms'
    where ms' = map (\(GMDecl n' _ _) -> n') ms

checkMethod :: TypeName -> [MethodName] -> MethodName -> ClassAnalyzer ()
checkMethod n ms m = when (count > 1) (throwError $ "Multiple definitions of method " ++ m ++ " in class " ++ n)
    where count = length . filter (== m) $ ms

checkCyclicInheritance :: (TypeName, Maybe TypeName) -> ClassAnalyzer ()
checkCyclicInheritance (_, Nothing) = return ()
checkCyclicInheritance (n, b) = checkInheritance n b [n]

checkInheritance :: TypeName -> Maybe TypeName -> [TypeName] -> ClassAnalyzer ()
checkInheritance _ Nothing _ = return ()
checkInheritance n (Just b) visited =
    do when (b `elem` visited) (throwError $ "Cyclic inheritance involving class " ++ n)
       next <- getBase b
       checkInheritance n next (b : visited)

getBase :: TypeName -> ClassAnalyzer (Maybe TypeName)
getBase n = gets superClass >>= \sc ->
    case lookup n sc of
        (Just b) -> return b
        Nothing -> throwError $ "ICE: Unknown class " ++ n

setMainClass :: (TypeName, [MethodDeclaration]) -> ClassAnalyzer ()
setMainClass (n, ms) = when ("main" `elem` ms') (gets mainClass >>= set)
    where ms' = map (\(GMDecl n' _ _) -> n') ms
          set :: Maybe TypeName -> ClassAnalyzer ()
          set (Just m) = throwError $ "Method main already defined in class " ++ m ++ " but redefined in class " ++ n
          set Nothing = modify $ \s -> s { mainClass = Just n }

checkMainMethod :: ClassAnalyzer ()
checkMainMethod = gets mainClass >>= \mc -> when (isNothing mc) (throwError "No main method defined")

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
       mapM_ setMainClass $ classMethods s
       checkMainMethod
       return $ GProg $ filter noBase p
    where noBase (GCDecl _ Nothing _ _) = True
          noBase _ = False

classAnalysis :: Program -> Either String (Program, CAState)
classAnalysis p = runExcept $ runStateT (runCA $ caProgram p) initialState