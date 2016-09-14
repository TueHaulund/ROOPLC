module PISA where

import Data.List (intercalate)

type Immediate = Integer
type Label = String

data Register = Reg Integer
    deriving (Eq)

data Offset = Lbl Label | Imm Immediate
    deriving (Show, Eq)

data Instruction = ADD Register Register
                 | ADDI Register Immediate
                 | ANDX Register Register Register
                 | ANDIX Register Register Immediate
                 | NORX Register Register Register
                 | NEG Register
                 | ORX Register Register Register
                 | ORIX Register Register Immediate
                 | RL Register Immediate
                 | RLV Register Register
                 | RR Register Immediate
                 | RRV Register Register
                 | SLLX Register Register Immediate
                 | SLLVX Register Register Register
                 | SRAX Register Register Immediate
                 | SRAVX Register Register Register
                 | SRLX Register Register Immediate
                 | SRLVX Register Register Register
                 | SUB Register Register
                 | XOR Register Register
                 | XORI Register Immediate
                 | BEQ Register Register Offset
                 | BGEZ Register Offset
                 | BGTZ Register Offset
                 | BLEZ Register Offset
                 | BLTZ Register Offset
                 | BNE Register Register Offset
                 | BRA Offset
                 | EXCH Register Register
                 | SWAPBR Register
                 | RBRA Offset
                 | START
                 | FINISH
                 | DATA Immediate
    deriving (Eq)

type Program = [(Maybe Label, Instruction)]

instance Show Register where
    show (Reg r) = "$" ++ show r

instance Show Instruction where
    show (ADD r1 r2) = unwords ["ADD   ", show r1, show r2]
    show (ADDI r i) = unwords ["ADDI  ", show r, show i]
    show (ANDX r1 r2 r3) = unwords ["ANDX  ", show r1, show r2, show r3]
    show (ANDIX r1 r2 i) = unwords ["ANDIX ", show r1, show r2, show i]
    show (NORX r1 r2 r3) = unwords ["NORX  ", show r1, show r2, show r3]
    show (NEG r) = unwords ["NEG   ", show r]
    show (ORX r1 r2 r3) = unwords ["ORX   ", show r1, show r2, show r3]
    show (ORIX r1 r2 i) = unwords ["ORIX  ", show r1, show r2, show i]
    show (RL r i) = unwords ["RL    ", show r, show i]
    show (RLV r1 r2) = unwords ["RLV   ", show r1, show r2]
    show (RR r i) = unwords ["RR    ", show r, show i]
    show (RRV r i) = unwords ["RRV   ", show r, show i]
    show (SLLX r1 r2 i) = unwords ["SLLX  ", show r1, show r2, show i]
    show (SLLVX r1 r2 r3) = unwords ["SLLVX ", show r1, show r2, show r3]
    show (SRAX r1 r2 i) = unwords ["SRAX  ", show r1, show r2, show i]
    show (SRAVX r1 r2 r3) = unwords ["SRAVX ", show r1, show r2, show r3]
    show (SRLX r1 r2 i) = unwords ["SRLX  ", show r1, show r2, show i]
    show (SRLVX r1 r2 r3) = unwords ["SRLVX ", show r1, show r2, show r3]
    show (SUB r1 r2) = unwords ["SUB   ", show r1, show r2]
    show (XOR r1 r2) = unwords ["XOR   ", show r1, show r2]
    show (XORI r i) = unwords ["XORI  ", show r, show i]
    show (BEQ r1 r2 o) = unwords ["BEQ   ", show r1, show r2, show o]
    show (BGEZ r o) = unwords ["BGEZ  ", show r, show o]
    show (BGTZ r o) = unwords ["BGTZ  ", show r, show o]
    show (BLEZ r o) = unwords ["BLEZ  ", show r, show o]
    show (BLTZ r o) = unwords ["BLTZ  ", show r, show o]
    show (BNE r1 r2 o) = unwords ["BNE   ", show r1, show r2, show o]
    show (BRA o) = unwords ["BRA   ", show o]
    show (EXCH r1 r2) = unwords ["EXCH  ", show r1, show r2]
    show (SWAPBR r) = unwords ["SWAPBR", show r]
    show (RBRA o) = unwords ["RBRA  ", show o]
    show START = "START "
    show FINISH = "FINISH"
    show (DATA i) = unwords ["DATA  ", show i]

showProgram :: Program -> String
showProgram xs = intercalate "\n" $ map showLine xs
    where showLine (Nothing, i) = spaces 15 ++ show i
          showLine (Just l, i) = l ++ ":" ++ spaces (14 - length l) ++ show i
          spaces :: (Int -> String)
          spaces n = [1..n] >> " "

writeProgram :: String -> Program -> IO ()
writeProgram file p = writeFile file $ showProgram p