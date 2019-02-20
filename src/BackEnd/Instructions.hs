{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module BackEnd.Instructions where
-- The arm instruction set datatypes
import Data.Int
import Generics.Deriving.Show
import GHC.Generics hiding(R1)
import Data.Char
import BackEnd.Temp

data REG = PC | LR | SP | R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 |
           R11 | R12 | RTEMP Temp deriving Show

data OP =  R REG | IMM Int | CHR Char | NoOP -- immediate values need to be restricted size
type Lable = String
data Suffix = S | NoSuffix -- if specified, update the flags
data Opt = OPT REG | NoReg -- Optional Register
data SLType = B_ | SB | H | SH | W deriving Generic -- only saw sb in ref compiler
data SLOP2 = MSG String | PRE REG Int | POST REG Int | Imm REG Int -- IF int is zero dont show

{- Cond not included here :  HS/CS LO/CC because I don't knwo which to use-}
data Cond = EQ | NE | MI | PL | VS | VC | HI | LS | GE | LT | GT | LE | AL
            deriving Generic

instance GShow Cond

instance Show Cond where
  show AL = ""
  show x = gshow x

instance Header Cond where
  show_ = show

instance GShow SLType

instance Show SLType where
  show W = ""
  show B_ = "B"
  show x = gshow x

instance Header SLType where
  show_ = show

instance Show OP where
  show (IMM int) = ", #" ++ show int
  show (R reg) = ", " ++ show reg
  show (CHR chr) = ", #" ++ show chr
  show _ = ""
-- since op is always at the end of an assembly I included the spaces and comma here

instance Show Suffix where
  show S = "S"
  show _ = ""

instance Header Suffix where
  show_ = show

instance Show Opt where
  show (OPT reg) = show reg
  show _ = ""

instance Show SLOP2 where
  show (Imm reg 0) = "[" ++ show reg ++ "]"
  show (Imm reg int) = "[" ++ show reg ++ ", #" ++ show int ++ "]"
  show (PRE reg int) = show (Imm reg int) ++ "!"
  show (POST reg int) = "[" ++ show reg ++ "]" ++ ", #" ++ show int
  show (MSG str) = ", =" ++ str  -- did not include shift *

{- Instructions not included here: cpy adc abc neg mul cmn mul
   bic mvn tst bx blx ldrh ldrsh ldrb ldmia strh stmia cps setend
   rev rev16 revsh svc bkpt sxth sxtb uxth uxtb -}

{- Instructions that are here but not in the instruction summary:
   smull (suffix) (cond) REG REG REG REG -}

{- MOVS seems to be not in the reference compiler-}

class Show a => Header a where
  show_ :: a -> String
  show_ a = (filter (not.isSpace) (show a)) ++ " "

data Calc = ADD Suffix Cond | SUB Suffix Cond | AND Suffix Cond |
            EOR Suffix Cond | ORR Suffix Cond deriving (Show, Header)
data Simple = CMP Cond | MOV Cond deriving (Show, Header)
data Shift = LSL Suffix Cond | ASR Suffix Cond | LSR Suffix Cond | ROR Suffix Cond deriving (Show, Header)
data Branch = B Cond | BL Cond deriving (Show, Header)
data StackOP = PUSH Cond | POP Cond deriving (Show, Header)
data Calc2 = SMULL Suffix Cond deriving (Show, Header)
data SL = LDR SLType Cond | STR SLType Cond deriving (Show, Header)
data Calc3 = SDIV Cond deriving (Show, Header)

data Instr = CB_ Calc REG REG OP | MC_ Simple REG OP |
             SHIFT_ Shift REG REG REG | BRANCH_ Branch Lable |
             STACK_ StackOP [REG] | C2_ Calc2 REG REG REG REG |
             S_ SL REG SLOP2 | C3_ Calc3 REG REG REG

instance Show Instr where
  show (CB_ c r1 r2 op) = (show_ c)  ++ (show r1) ++ ", " ++ (show r2) ++ (show op)
  show (MC_ s r op) = (show_ s)  ++ (show r) ++ (show op)
  show (SHIFT_ s r1 r2 r3) = (show_ s)  ++ (show r1) ++ ", " ++ (show r2)  ++ ", " ++ (show r3)
  show (BRANCH_ b l) = (show_ b)  ++ l
  show (STACK_ s (r:regs)) = show_ s  ++ "{" ++ show r ++ (concatMap (\x -> ", " ++ show x) regs) ++ "}"
  show (C2_ c r1 r2 r3 r4) = (show_ c) ++ (show r1) ++ ", " ++ (show r2)  ++ ", " ++ (show r3) ++ ", " ++ (show r4)
  show (S_ s r1 op) = (show_ s) ++ (show r1) ++ ", " ++ (show op)
  show (C3_ c r1 r2 r3) = (show_ c) ++ (show r1) ++ ", " ++ (show r2)  ++ ", " ++ (show r3)

{- Sample instruction representations -}
sample1 = CB_ (ADD NoSuffix AL) R1 R2 (IMM 3)
sample2 = MC_ (CMP BackEnd.Instructions.GT) R7 (CHR 'a')
sample3 = SHIFT_ (LSL NoSuffix AL) R0 R1 R2
sample4 = BRANCH_ (B BackEnd.Instructions.EQ) "Hello"
sample5 = STACK_ (POP BackEnd.Instructions.LS) [R1, R2, PC, SP]
sample6 = C2_ (SMULL S AL) R12 R11 R10 R9
sample7 = S_ (LDR B_ AL) R9 (POST R5 3)
