{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ClassAnalyzer (classAnalysis, CAState(..)) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import Data.Maybe

import AST

data CAState =
    CAState {
        classes :: [TypeName],
        inherits :: [(TypeName, Maybe TypeName)],
        fields :: [(TypeName, [VariableDeclaration])],
        methods :: [(TypeName, [MethodDeclaration])],
        mainClass :: Maybe TypeName
    } deriving (Show, Eq)

newtype ClassAnalyzer a = ClassAnalyzer { runCA :: StateT CAState (Except String) a }
    deriving (Functor, Applicative, Monad, MonadState CAState, MonadError String)

checkDuplicateClasses :: TypeName -> ClassAnalyzer ()
checkDuplicateClasses n =
    do cs <- gets classes
       when (count cs > 1) (throwError $ "Multiple definitions of class " ++ n)
    where count = length . filter (== n)

checkBaseClass :: (TypeName, Maybe TypeName) -> ClassAnalyzer ()
checkBaseClass (_, Nothing) = return ()
checkBaseClass (n, Just n') =
    do when (n == n') (throwError $ "Class " ++ n ++ " cannot inherit from itself")
       cs <- gets classes
       when (n' `notElem` cs) (throwError $ "Class " ++ n ++ " cannot inherit from unknown class " ++ n')

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
getBase n = gets inherits >>= \is ->
    case lookup n is of
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
initialState = CAState { classes = [], inherits = [], fields = [], methods = [], mainClass = Nothing }

insertClass :: ClassDeclaration -> ClassAnalyzer ()
insertClass (GCDecl n b fs ms) = modify $ \s ->
    s { classes = n : classes s,
        inherits = (n, b) : inherits s,
        fields = (n, fs) : fields s,
        methods = (n, ms) : methods s }

caClass :: ClassDeclaration -> ClassAnalyzer PProgram
caClass c@(GCDecl n _ _ ms) = insertClass c >> return (zip (repeat n) ms)

caProgram :: Program -> ClassAnalyzer PProgram
caProgram (GProg p) =
    do r <- concat <$> mapM caClass p
       s <- get
       mapM_ checkDuplicateClasses $ classes s
       mapM_ checkBaseClass $ inherits s
       mapM_ checkDuplicateFields $ fields s
       mapM_ checkDuplicateMethods $ methods s
       mapM_ checkCyclicInheritance $ inherits s
       mapM_ setMainClass $ methods s
       checkMainMethod
       return r

classAnalysis :: Program -> Either String (PProgram, CAState)
classAnalysis p = runExcept $ runStateT (runCA $ caProgram p) initialState