module BackEnd.IR where

import BackEnd.Temp as Temp

data Exp = CONSTI Int              -- constant int
         | CONSTC Char
         | NAME Temp.Label        -- symbolic constant
         | TEMP Temp.Temp         -- symbolic register
         | BINEXP BOp Exp Exp
         | MEM Exp          -- memory address at exp
         | CALL Exp [Exp]   -- Call function address [values]
         | ESEQ Stm Exp     -- evaluated for side effect
           deriving (Eq, Show)

data Stm = MOV Exp Exp -- move values to address or register
         | JUMP Exp [Temp.Label]  -- Jump: expression to evaluate, list of possible jump destination
         | CJUMP ROp Exp Exp Temp.Label Temp.Label
         | SEQ Stm Stm -- sequence of statement
         | LABEL Temp.Label -- target of jump
         | PUSH Exp
         | POP Exp
         | NOP
          deriving (Eq, Show)

--         IR    ARM type
data BOp = PLUS --CALC1
         | MINUS --CALC1
         | MUL --CALC2
         | DIV --CALC3
         | AND --CALC1
         | OR  --CALC1
         | LSHIFT --SHIFT
         | RSHIFT --SHIFT
         | MOD  --hand written
         deriving (Eq, Show)
         
data ROp = EQ
         | NE
         | LT
         | LE
         | GT
         | GE
           deriving (Eq, Show)
