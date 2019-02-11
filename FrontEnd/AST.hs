module FrontEnd.AST where

import System.IO
import Data.List
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

import Text.ParserCombinators.Parsec.Pos

import FrontEnd.Lexer


data Program a = Program [FuncF a] (StatListF a) deriving (Eq, Show)

data Func a = Func Type (IdentF a) [ParamF a] (StatListF a)
                deriving (Eq, Show)

data Param a = Param Type (IdentF a) deriving (Eq, Show)

data StatList a = StatList [StatF a] deriving (Eq, Show)

data Stat a = Declare Type (IdentF a) (ExprF a)
            | Assign (ExprF a) (ExprF a)
            | Return (ExprF a)
            | Exit (ExprF a)
            | FuncStat (FuncAppF a)
            | If (ExprF a) (StatListF a) (StatListF a)
            | While (ExprF a) (StatListF a)
            | Subroutine (StatListF a)
            deriving (Eq, Show)


data Type = TInt
          | TBool
          | TChar
          | TStr
          | TArray Type
          | TPair Type Type
          | TAny
          | TFunc Type [Type] -- return type, param type
          | Void
          | None
          | TRef Type
          | T -- similar to type parameter

data Expr a = IntLiter Integer
            | BoolLiter Bool
            | CharLiter Char
            | StringLiter String
            | IdentExpr (IdentF a)
            | Null
            | BracketExpr (ExprF a)
            | ArrayElem (IdentF a) [ExprF a]
            | ArrayLiter [ExprF a]
            | FuncExpr (FuncAppF a)
            | NewPair (ExprF a) (ExprF a)
            | PairElemFst (ExprF a)
            | PairElemSnd (ExprF a)
            deriving (Eq, Show)


data FuncApp a = FuncApp (IdentF a) [ExprF a] deriving (Eq, Show)


data Ident a = Ident String deriving (Eq)

--data ArrayElem a = ArrayElem (IdentF a) [ExprF a] deriving (Eq, Show)

data Ann f = Ann f (SourcePos, Type)

data Parse = Parse deriving (Show, Eq)
data Semantic = Semantic deriving (Show, Eq)

type ProgramF a = Ann  (Program a)
type FuncF a = Ann  (Func a)
type StatF a = Ann  (Stat a)
type ExprF a = Ann  (Expr a)
type IdentF a = Ann  (Ident a)
type StatListF a = Ann (StatList a)
type ParamF a = Ann (Param a)
type FuncAppF a = Ann (FuncApp a)


-- (functionName, allowed type, [type parameter matching],
--  return type)
builtInFunc :: [(String, ([Type], [Type], Type))]
builtInFunc =
  [("skip",    ([],                             [],        Void)),
   ("read",    ([TInt, TChar],                  [TRef T],  Void)),
   ("free",    ([TArray TAny, TPair TAny TAny], [TRef T],  Void)),
   ("print",   ([TAny],                         [T],       Void)),
   ("println", ([TAny],                         [T],       Void)),
   ("newpair", ([],                             [],        TPair TAny TAny)),
   ("fst",     ([TAny],                         [TPair T TAny], T)),
   ("snd",     ([TAny],                         [TPair TAny T], TAny))]



instance (Show f) => Show (Ann f) where
  show (Ann f (_, t)) = show f

instance (Eq f) => Eq (Ann f) where
  (Ann f1 _) == (Ann f2 _) = f1 == f2

instance Eq (Type) where
  TInt == TInt = True
  TBool == TBool = True
  TChar == TChar = True
  TStr == TStr = True
  (TArray t1) == (TArray t2) = t1 == t2
  (TPair ft1 st1) == (TPair ft2 st2) = (ft1 == ft2) &&
                                       (st1 == st2)
  TAny == _ = True
  _ == TAny = True
  Void == Void = True
  (TFunc t1 ts1) == (TFunc t2 ts2) = (t1 == t2) &&
                                     (ts1 == ts2)
  _ == _ = False

instance Show (Type) where
  show TInt = "TInt"
  show TBool = "TBool"
  show TChar = "TChar"
  show TStr = "TStr"
  show (TArray t) = "TArray [" ++ show t ++ "]"
  show (TPair t1 t2) = "TPair (" ++ show t1 ++ ", " ++ show t2
                       ++ ")"
  show TAny = "Any"
  show Void = "Void"
  show (TFunc t ts) = "TFunc (" ++ show t ++ ") " ++
    "(" ++ intersperse ',' (concat $ map show ts) ++ ")"
  show _ = "deal with it"

instance Show (Ident a) where
  show (Ident s) = show s


