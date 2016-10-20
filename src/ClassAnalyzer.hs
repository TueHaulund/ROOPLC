{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}

module ClassAnalyzer (classAnalysis, CAState(..)) where

import Data.Maybe
import Data.List

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import AST

type Size = Integer

data CAState =
    CAState {
        classes :: [(TypeName, ClassDeclaration)],
        subClasses :: [(TypeName, [TypeName])],
        superClasses :: [(TypeName, [TypeName])],
        classSize :: [(TypeName, Size)],
        classMethods :: [(TypeName, [MethodDeclaration])],
        mainClass :: Maybe TypeName
    } deriving (Show, Eq)

newtype ClassAnalyzer a = ClassAnalyzer { runCA :: StateT CAState (Except String) a }
    deriving (Functor, Applicative, Monad, MonadState CAState, MonadError String)

getClass :: TypeName -> ClassAnalyzer ClassDeclaration
getClass n = gets classes >>= \cs ->
    case lookup n cs of
        (Just c) -> return c
        Nothing -> throwError $ "ICE: Unknown class " ++ n

getBaseClass :: TypeName -> ClassAnalyzer (Maybe TypeName)
getBaseClass n = getClass n >>= getBase
    where getBase (GCDecl _ b _ _) = return b

checkDuplicateClasses :: ClassDeclaration -> ClassAnalyzer ()
checkDuplicateClasses (GCDecl n _ _ _) = gets classes >>= \cs ->
    when (count cs > 1) (throwError $ "Multiple definitions of class " ++ n)
    where count = length . filter ((== n) . fst)

checkBaseClass :: ClassDeclaration -> ClassAnalyzer ()
checkBaseClass (GCDecl _ Nothing _ _) = return ()
checkBaseClass (GCDecl n (Just b) _ _) =
    do when (n == b) (throwError $ "Class " ++ n ++ " cannot inherit from itself")
       cs <- gets classes
       when (isNothing $ lookup b cs) (throwError $ "Class " ++ n ++ " cannot inherit from unknown class " ++ b)

checkDuplicateFields :: ClassDeclaration -> ClassAnalyzer ()
checkDuplicateFields (GCDecl n _ fs _) = mapM_ checkField fs
    where count v = length . filter (\(GDecl _ v') -> v' == v) $ fs
          checkField (GDecl _ v) = when (count v > 1) (throwError $ "Multiple declarations of field " ++ v ++ " in class " ++ n)

checkDuplicateMethods :: ClassDeclaration -> ClassAnalyzer ()
checkDuplicateMethods (GCDecl n _ _ ms) = mapM_ checkMethod ms'
    where ms' = map (\(GMDecl n' _ _) -> n') ms
          count m = length . filter (== m) $ ms'
          checkMethod m = when (count m > 1) (throwError $ "Multiple definitions of method " ++ m ++ " in class " ++ n)

checkCyclicInheritance :: ClassDeclaration -> ClassAnalyzer ()
checkCyclicInheritance (GCDecl _ Nothing _ _) = return ()
checkCyclicInheritance (GCDecl n b _ _) = checkInheritance b [n]
    where checkInheritance Nothing _ = return ()
          checkInheritance (Just b') visited =
              do when (b' `elem` visited) (throwError $ "Cyclic inheritance involving class " ++ n)
                 next <- getBaseClass b'
                 checkInheritance next (b' : visited)

setMainClass :: ClassDeclaration -> ClassAnalyzer ()
setMainClass (GCDecl n _ _ ms) = when ("main" `elem` ms') (gets mainClass >>= set)
    where ms' = map (\(GMDecl n' _ _) -> n') ms
          set (Just m) = throwError $ "Method main already defined in class " ++ m ++ " but redefined in class " ++ n
          set Nothing = modify $ \s -> s { mainClass = Just n }

initialState :: CAState
initialState =
    CAState {
        classes = [],
        subClasses = [],
        superClasses = [],
        classSize = [],
        classMethods = [],
        mainClass = Nothing }

setClasses :: ClassDeclaration -> ClassAnalyzer ()
setClasses c@(GCDecl n _ _ _) = modify $ \s -> s { classes = (n, c) : classes s }

setSubClasses :: ClassDeclaration -> ClassAnalyzer ()
setSubClasses (GCDecl n b _ _) = modify (\s -> s { subClasses = (n, []) : subClasses s }) >> addSubClass n b

addSubClass :: TypeName -> Maybe TypeName -> ClassAnalyzer ()
addSubClass _ Nothing = return ()
addSubClass n (Just b) = gets subClasses >>= \sc ->
    case lookup b sc of
        Nothing -> modify $ \s -> s { subClasses = (b, [n]) : sc }
        (Just sc') -> modify $ \s -> s { subClasses = (b, n : sc') : delete (b, sc') sc }

setSuperClasses :: ClassDeclaration -> ClassAnalyzer ()
setSuperClasses (GCDecl n _ _ _) = gets subClasses >>= \sc ->
    modify $ \s -> s { superClasses = (n, map fst $ filter (\(_, sub) -> n `elem` sub) sc) : superClasses s }

getClassSize :: ClassDeclaration -> ClassAnalyzer Size
getClassSize (GCDecl _ Nothing fs _) = return $ 1 + genericLength fs
getClassSize (GCDecl _ (Just b) fs _) = getClass b >>= getClassSize >>= \sz -> return $ sz + genericLength fs

setClassSize :: ClassDeclaration -> ClassAnalyzer ()
setClassSize c@(GCDecl n _ _ _) = getClassSize c >>= \sz ->
    modify $ \s -> s { classSize = (n, sz) : classSize s }

resolveClassMethods :: ClassDeclaration -> ClassAnalyzer [MethodDeclaration]
resolveClassMethods (GCDecl _ Nothing _ ms) = return ms
resolveClassMethods (GCDecl n (Just b) _ ms) = getClass b >>= resolveClassMethods >>= combine
    where checkSignature (GMDecl m ps _, GMDecl m' ps' _) = when (m == m' && ps /= ps') (throwError $ "Method " ++ m ++ " in class " ++ n ++ " has invalid method signature")
          compareName (GMDecl m _ _) (GMDecl m' _ _) = m == m'
          combine ms' = mapM_ checkSignature ((,) <$> ms <*> ms') >> return (unionBy compareName ms ms')

setClassMethods :: ClassDeclaration -> ClassAnalyzer ()
setClassMethods c@(GCDecl n _ _ _) = resolveClassMethods c >>= \cm ->
    modify $ \s -> s { classMethods = (n, cm) : classMethods s }

caProgram :: Program -> ClassAnalyzer Program
caProgram (GProg p) =
    do mapM_ setClasses p
       mapM_ setSubClasses p
       mapM_ setSuperClasses p
       mapM_ setClassSize p
       mapM_ setClassMethods p
       mapM_ checkDuplicateClasses p
       mapM_ checkDuplicateFields p
       mapM_ checkDuplicateMethods p
       mapM_ checkBaseClass p
       mapM_ checkCyclicInheritance p
       mapM_ setMainClass p
       mc <- gets mainClass
       when (isNothing mc) (throwError "No main method defined")
       return $ GProg rootClasses
    where rootClasses = filter noBase p
          noBase (GCDecl _ Nothing _ _) = True
          noBase _ = False

classAnalysis :: Program -> Except String (Program, CAState)
classAnalysis p = runStateT (runCA $ caProgram p) initialState