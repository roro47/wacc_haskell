module BackEnd.Instructions where
-- The arm instruction set datatypes
import Data.Int


data REG = PC | LR | SP | R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 |
           R11 | R12
data OP =  IMM_OFS REG Int8 | MSG Int | R REG | IMM Int
ata Lable = Lable String
data Suffix = S | None  -- if specified, update the flags
data REGt = REGT REG | EMPTY -- REGtional REGerand
data SLType = B | SB | H | SH | W -- only saw sb in ref compiler
data SLOP2 = PRE Reg Int | POST Reg Int | Imm Reg Int -- IF int is zero dont show


{- Cond not included here :  HS/CS LO/CC because I don't knwo which to use-}
data Cond = EQ | NE | MI | PL | VS | VC | HI | LS | GE | LT | GT | LE | AL

{- Instructions not included here: cpy adc abc neg mul cmn mul
   bic mvn tst bx blx ldrh ldrsh ldrb ldmia strh stmia cps setend
   rev rev16 revsh svc bkpt sxth sxtb uxth uxtb -}

{- Instructions that are here but not in the instruction summary:
   smull (suffix) (cond) REG REG REG REG -}


{- MOVS seems to be not in the reference compiler-}
data Instr = ADD Suffix Cond REG REG OP | MOV Cond REG OP | SUB Suffix Cond REG REG OP |
             CMP Cond REG OP | AND Suffix Cond REG REG Op | EOR Suffix Cond REG REG Op |
             ORR Suffix Cond REG REG Op | LSL  Suffix Cond REG REG REG |
             LSR Suffix Cond REG REG REG | ASR Suffix Cond REG REG Reg |
             ROR Suffix Cond REG REG Reg | B Cond Lable | BL Cond Lable |
             PUSH Cond [REG] | POP Cond [REG] | SMULL Suffix Cond REG REG REG REG |
             LDR SLType Cond REG SLOP2 | STR SLType Cond REG SLOP2
