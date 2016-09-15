{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ClassAnalyzer (classAnalysis) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import Data.Maybe

import AST

type ClassType = (Maybe TypeName, [VariableDeclaration], [(MethodName, [VariableDeclaration])])

data CAState =
    CAState {
        classMap :: [(TypeName, ClassType)],
        mainClass :: Maybe TypeName
    } deriving (Show, Eq)

newtype ClassAnalyzer a = ClassAnalyzer { runCA :: StateT CAState (Except String) a }
    deriving (Functor, Applicative, Monad, MonadState CAState, MonadError String)

checkDuplicateClasses :: (TypeName, ClassType) -> ClassAnalyzer ()
checkDuplicateClasses (n, _) =
    do cm <- gets classMap
       when (count cm > 1) (throwError $ "Multiple definitions of class " ++ n)
    where count = length . filter (\(n', _) -> n' == n)

checkBaseClass :: (TypeName, ClassType) -> ClassAnalyzer ()
checkBaseClass (_, (Nothing, _, _)) = return ()
checkBaseClass (n, (Just n', _, _)) =
    do cm <- gets classMap
       when (n == n') (throwError $ "Class " ++ n ++ " cannot inherit from itself")
       when (isNothing $ lookup n' cm) (throwError $ "Class " ++ n ++ " cannot inherit from unknown class " ++ n')

checkDuplicateFields :: (TypeName, ClassType) -> ClassAnalyzer ()
checkDuplicateFields (n, (_, fs, _)) = mapM_ (checkField n fs) fs

checkField :: TypeName -> [VariableDeclaration] -> VariableDeclaration -> ClassAnalyzer ()
checkField n fs (GDecl _ v) = when (count > 1) (throwError $ "Multiple declarations of field " ++ v ++ " in class " ++ n)
    where count = length . filter (\(GDecl _ v') -> v' == v) $ fs

checkDuplicateMethods :: (TypeName, ClassType) -> ClassAnalyzer ()
checkDuplicateMethods (n, (_, _, ms)) = mapM_ (checkMethod n ms') ms'
    where ms' = map fst ms

checkMethod :: TypeName -> [MethodName] -> MethodName -> ClassAnalyzer ()
checkMethod n ms m = when (count > 1) (throwError $ "Multiple definitions of method " ++ m ++ " in class " ++ n)
    where count = length . filter (== m) $ ms

checkCyclicInheritance :: (TypeName, ClassType) -> ClassAnalyzer ()
checkCyclicInheritance (_, (Nothing, _, _)) = return ()
checkCyclicInheritance (n, c) = checkInheritance n c [n]

checkInheritance :: TypeName -> ClassType -> [TypeName] -> ClassAnalyzer ()
checkInheritance _ (Nothing, _, _) _ = return ()
checkInheritance n (Just b, _, _) visited =
    do when (b `elem` visited) (throwError $ "Cyclic inheritance involving class " ++ n)
       next <- lookupClass b
       checkInheritance n next (b:visited)

lookupClass :: TypeName -> ClassAnalyzer ClassType
lookupClass n = gets classMap >>= \cm ->
    case lookup n cm of
        (Just c) -> return c
        Nothing -> throwError $ "Unknown class " ++ n

setMainClass :: (TypeName, ClassType) -> ClassAnalyzer ()
setMainClass (n, (_, _, ms)) = when ("main" `elem` ms') (gets mainClass >>= set)
    where ms' = map fst ms
          set :: Maybe TypeName -> ClassAnalyzer ()
          set (Just m) = throwError $ "Method main already defined in class " ++ m ++ " but redefined in class " ++ n
          set Nothing = modify $ \s -> s { mainClass = Just n }

checkMainMethod :: ClassAnalyzer ()
checkMainMethod = get >>= checkState
    where checkState :: CAState -> ClassAnalyzer ()
          checkState (CAState _ Nothing) = throwError "No main method defined"
          checkState (CAState _ (Just _)) = return ()

initialState :: CAState
initialState = CAState { classMap = [], mainClass = Nothing }

insertClass :: ClassDeclaration -> ClassAnalyzer ()
insertClass (GCDecl n b fs ms) = modify $ \s -> s { classMap = (n, (b, fs, ms')) : classMap s }
    where ms' = map (\(GMDecl mn ps _) -> (mn, ps)) ms

caClass :: ClassDeclaration -> ClassAnalyzer [(TypeName, MethodDeclaration)]
caClass c@(GCDecl n _ _ ms) = insertClass c >> return (zip (repeat n) ms)

caProgram :: Program -> ClassAnalyzer [(TypeName, MethodDeclaration)]
caProgram (GProg p) =
    do r <- concat <$> mapM caClass p
       cm <- gets classMap
       mapM_ checkDuplicateClasses cm
       mapM_ checkBaseClass cm
       mapM_ checkDuplicateFields cm
       mapM_ checkDuplicateMethods cm
       mapM_ checkCyclicInheritance cm
       mapM_ setMainClass cm
       checkMainMethod
       return r

classAnalysis :: Program -> Either String ([(TypeName, MethodDeclaration)], CAState)
classAnalysis p = runExcept $ runStateT (runCA $ caProgram p) initialState