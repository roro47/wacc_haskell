module BackEnd.IR where

import BackEnd.Temp as Temp

data Exp = CONST Int              -- constant int
         | TEMP Temp.Temp         -- symbolic register
         | BINEXP BOp Exp Exp
         | MEM Exp          -- memory address at exp
         | CALL Exp [Exp]   -- Call function address [values]
           deriving (Eq, Show)

data Stm = MOVE Exp Exp -- move values to address or register
         | JUMP Exp [Temp.Label]  -- Jump: expression to evaluate, list of possible jump destination
         | CJUMP ROp Exp Exp Temp.Label Temp.Label
         | SEQ Stm Stm -- sequence of statement
         | LABEL Temp.Label -- target of jump
          deriving (Eq, Show)

data BOp = PLUS
         | MINUX
         | MUL
         | DIV
         | AND
         | OR
         | LSHIFT
         | RSHIFT
         deriving (Eq, Show)
data ROp = EQ
         | NE
         | LT
         | GT
         | LT
         | GE
           deriving (Eq, Show)
