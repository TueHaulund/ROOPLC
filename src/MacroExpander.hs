{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MacroExpander (expandMacros) where

import Data.Maybe
import Data.List

import Control.Monad.Reader
import Control.Monad.Except
import Control.Arrow

import AST hiding (Program, GProg, Offset)
import PISA

import ScopeAnalyzer
import ClassAnalyzer

type Size = Integer
type Address = Integer
type Offset = Integer

data MEState =
    MEState {
        addressTable :: [(Label, Address)],
        sizeTable :: [(TypeName, Size)],
        offsetTable :: [(TypeName, [(MethodName, Offset)])],
        programSize :: Size
    } deriving (Show, Eq)

newtype MacroExpander a = MacroExpander { runME :: ReaderT MEState (Except String) a }
    deriving (Functor, Applicative, Monad, MonadReader MEState, MonadError String)

getOffsetTable :: SAState -> [(TypeName, [(MethodName, Offset)])]
getOffsetTable s = map (second (map toOffset)) indexedVT
    where indexedVT = map (second $ zip [0..]) $ virtualTables s
          toOffset (i, m) = (getName $ lookup m $ symbolTable s, i)
          getName (Just (Method _ n)) = n
          getName _ = error "ICE: Invalid method index"

initialState :: MProgram -> SAState -> MEState
initialState (GProg p) s =
    MEState {
        addressTable = mapMaybe toPair $ zip [0..] p,
        sizeTable = (classSize . caState) s,
        offsetTable = getOffsetTable s,
        programSize = genericLength p }
    where toPair (a, (Just l, _)) = Just (l, a)
          toPair _ = Nothing

getAddress :: Label -> MacroExpander Address
getAddress l = asks addressTable >>= \at ->
    case lookup l at of
        (Just i) -> return i
        Nothing -> throwError $ "ICE: Unknown label " ++ l

getSize :: TypeName -> MacroExpander Size
getSize tn = asks sizeTable >>= \st ->
    case lookup tn st of
        (Just s) -> return s
        Nothing -> throwError $ "ICE: Unknown type " ++ tn

getOffset :: TypeName -> MethodName -> MacroExpander Offset
getOffset tn mn = asks offsetTable >>= \ot ->
    case lookup tn ot of
        Nothing -> throwError $ "ICE: Unknown type " ++ tn
        (Just mo) -> case lookup mn mo of
                         Nothing -> throwError $ "ICE: Unknown method " ++ mn
                         (Just o) -> return o

meMacro :: Macro -> MacroExpander Integer
meMacro (Immediate i) = return i
meMacro (AddressMacro l) = getAddress l
meMacro (SizeMacro tn) = getSize tn
meMacro (OffsetMacro tn mn) = getOffset tn mn
meMacro ProgramSize = asks programSize

meInstruction :: MInstruction -> MacroExpander Instruction
meInstruction (ADD r1 r2) = return $ ADD r1 r2
meInstruction (ADDI r m) = ADDI r <$> meMacro m
meInstruction (ANDX r1 r2 r3) = return $ ANDX r1 r2 r3
meInstruction (ANDIX r1 r2 m) = ANDIX r1 r2 <$> meMacro m
meInstruction (NORX r1 r2 r3) = return $ NORX r1 r2 r3
meInstruction (NEG r) = return $ NEG r
meInstruction (ORX r1 r2 r3) = return $ ORX r1 r2 r3
meInstruction (ORIX r1 r2 m) = ORIX r1 r2 <$> meMacro m
meInstruction (RL r m) = RL r <$> meMacro m
meInstruction (RLV r1 r2 ) = return $ RLV r1 r2
meInstruction (RR r m) = RR r <$> meMacro m
meInstruction (RRV r1 r2 ) = return $ RRV r1 r2
meInstruction (SLLX r1 r2 m) = SLLX r1 r2 <$> meMacro m
meInstruction (SLLVX r1 r2 r3) = return $ SLLVX r1 r2 r3
meInstruction (SRAX r1 r2 m) = SRAX r1 r2 <$> meMacro m
meInstruction (SRAVX r1 r2 r3) = return $ SRAVX r1 r2 r3
meInstruction (SRLX r1 r2 m) = SRLX r1 r2 <$> meMacro m
meInstruction (SRLVX r1 r2 r3) = return $ SRLVX r1 r2 r3
meInstruction (SUB r1 r2) = return $ SUB r1 r2
meInstruction (XOR r1 r2) = return $ XOR r1 r2
meInstruction (XORI r m) = XORI r <$> meMacro m
meInstruction (BEQ r1 r2 l) = return $ BEQ r1 r2 l
meInstruction (BGEZ r l) = return $ BGEZ r l
meInstruction (BGTZ r l) = return $ BGTZ r l
meInstruction (BLEZ r l) = return $ BLEZ r l
meInstruction (BLTZ r l) = return $ BLTZ r l
meInstruction (BNE r1 r2 l) = return $ BNE r1 r2 l
meInstruction (BRA l) = return $ BRA l
meInstruction (EXCH r1 r2) = return $ EXCH r1 r2
meInstruction (SWAPBR r) = return $ SWAPBR r
meInstruction (RBRA l) = return $ RBRA l
meInstruction START = return START
meInstruction FINISH = return FINISH
meInstruction (DATA m) = DATA <$> meMacro m
meInstruction (SUBI r m) = SUBI r <$> meMacro m

meProgram :: MProgram -> MacroExpander Program
meProgram (GProg p) = GProg <$> mapM expandPair p
    where expandPair (l, i) = (,) l <$> meInstruction i

expandMacros :: (MProgram, SAState) -> Except String Program
expandMacros (p, s) = runReaderT (runME $ meProgram p) $ initialState p s