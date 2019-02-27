module BackEnd.IR where

import Data.Tree
import Data.Tree.Pretty
import Prelude hiding(NE, EQ, GT, GE, LT, LE)
import BackEnd.Temp as Temp

data Exp = CONSTI Int              -- constant int
         | CONSTC Char
         | NAME Temp.Label        -- symbolic constant
         | TEMP Temp.Temp         -- symbolic register
         | BINEXP BOp Exp Exp
         | MEM Exp          -- memory address at exp
         | CALL Exp [Exp]   -- Call function address [values]
         | ESEQ Stm Exp     -- evaluated for side effect
           deriving (Eq)

data Stm = MOV Exp Exp -- move values to address or register
         | JUMP Exp [Temp.Label]  -- Jump: expression to evaluate, list of possible jump destination
         | EXP Exp
         | CJUMP ROp Exp Exp Temp.Label Temp.Label
         | SEQ Stm Stm -- sequence of statement
         | LABEL Temp.Label -- target of jump
         | PUSH Exp
         | POP Exp
         | NOP
          deriving (Eq)
cleanStmExp (BINEXP bop e1 e2) =
  BINEXP bop (cleanStmExp e1) (cleanStmExp e2)
cleanStmExp (MEM exp) = MEM (cleanStmExp exp)
cleanStmExp (CALL e1 es) = CALL (cleanStmExp e1) (map cleanStmExp es)
cleanStmExp (ESEQ stm e) = ESEQ (cleanStm stm) (cleanStmExp e)
cleanStmExp e = e

cleanStm (SEQ NOP NOP) = NOP
cleanStm (SEQ s1 NOP) = cleanStm s1
cleanStm (SEQ NOP s2) = cleanStm s2
cleanStm s@(SEQ s1 s2)
  | s1' == NOP || s2' == NOP = cleanStm $ SEQ s1' s2'
  | s1' /= s1 || s2' /= s2 = cleanStm $ SEQ s1' s2'
  | otherwise = SEQ s1' s2'
  where s1' = cleanStm s1
        s2' = cleanStm s2
cleanStm s = s


class Treeable a where
  toTree :: a -> Tree String

instance Treeable Exp where
  toTree (CONSTI i) = Node ("CONSTI " ++ show i) []
  toTree (CONSTC c) = Node ("CONSTC " ++ show c) []
  toTree (NAME l) = Node ("NAME " ++ show l) []
  toTree (TEMP t)
    | t == 11 = Node ("FP (r11)") []
    | t == 13 = Node ("SP (r13)") []
    | otherwise = Node ("TEMP " ++ show t) []
  toTree (BINEXP bop e1 e2) = Node "BINEXP" [Node (show bop) [], toTree e1, toTree e2]
  toTree (MEM e) = Node ("MEM" ) [toTree e]
  toTree (CALL e es) = Node ("CALL ") ([toTree e] ++ map toTree es)
  toTree (ESEQ s e) = Node ("ESEQ") [toTree s, toTree e]

instance Treeable Stm where
  toTree (MOV e1 e2) = Node "MOV" [toTree e1, toTree e2]
  toTree (JUMP e labels) = Node "JUMP" ([toTree e] ++ map (\l -> Node l []) labels)
  toTree (CJUMP rop e1 e2 label1 label2) =
    Node "CJUMP" [Node (show rop) [toTree e1, toTree e2],
                  Node label1 [], Node label2 []]
  toTree (LABEL label) = Node "LABEL" [Node label []]
  toTree (EXP exp) = Node "EXP" [toTree exp]
  toTree (PUSH exp) = Node "PUSH" [toTree exp]
  toTree (POP exp) = Node "POP" [toTree exp]
  toTree (SEQ s1 s2) = Node "SEQ" [toTree s1, toTree s2]
  toTree NOP = Node "NOP" []

instance Show Exp where
  show e = drawTree $ toTree e

instance Show Stm where
  show stm = drawTree $ toTree stm


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
