module AST where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

import Text.ParserCombinators.Parsec.Pos

import Lexer

data Pass c = Pass 

data Program a = Program [FuncF a] (StatF a) deriving (Eq, Show)

data Func a = Func (TypeF a) (IdentF a) [ParamF a] (StatF a) 
                deriving (Eq, Show)
              

data Param a = Param (TypeF a) (IdentF a) deriving (Eq, Show)

data Stat a = Skip
            | Declare (TypeF a) (IdentF a) (AssignRHSF a)
            | Assign (AssignLHSF a) (AssignRHSF a)
            | Read (AssignLHSF a)
            | Free (ExprF a)
            | Return (ExprF a)
            | Exit (ExprF a) 
            | Print (ExprF a)
            | Println (ExprF a)
            | If (ExprF a) (StatF a) (StatF a)
            | While (ExprF a) (StatF a)
            | Subroutine (ProgramF a)
            | Seq (StatF a) (StatF a)
            deriving (Eq, Show)
            
data AssignLHS a = IdentLHS (IdentF a)
                 | ArrayElemLHS (ArrayElemF a)
                 | PairElemLHS (PairElemF a)
                 deriving (Eq, Show)

data AssignRHS a = ExprRHS (ExprF a)
                 | NewPair (ExprF a) (ExprF a)
                 | PairElemRHS (PairElemF a)
                 | Call (IdentF a) [ExprF a]
                 | ArrayLiter [ExprF a]
                 deriving (Eq, Show)

data PairElem a = PairElemFst (ExprF a)
                | PairElemSnd (ExprF a)
                deriving (Eq, Show)

data Type a = TInt
            | TBool
            | TChar
            | TStr
            | TArray (TypeF a)
            | TPair (TypeF a) (TypeF a)
            | Any
            deriving (Eq, Show)

data Expr a = LiterExpr (LiterF a)
            | IdentExpr (IdentF a)
            | ArrayExpr (ArrayElemF a)
            | UExpr UnaryOp (ExprF a)
            | BExpr BinaryOp (ExprF a) (ExprF a)
            | BracketExpr (ExprF a)
            deriving (Eq, Show)

data Liter a = IntLiter Integer
             | BoolLiter Bool
             | CharLiter Char
             | StringLiter String
             | Null
             | PairLiter
             deriving (Eq, Show)
           
data UnaryOp = Pos | Not | Neg | Len | Ord | Chr deriving (Eq, Show)

data BinaryOp = Mul | Div | Mod | Plus | Minus | G | GEq | L | LEq | Eq | NEq | And | Or deriving (Eq, Show)


data Ident a = Ident String deriving (Eq, Show)

data ArrayElem a = ArrayElem (IdentF a) [ExprF a] deriving (Eq, Show)

data Ann f a = Ann f a deriving (Eq, Show)

type ProgramF a = Ann (Program a) a
type FuncF a = Ann (Func a) a
type ParamF a = Ann (Param a) a
type StatF a = Ann (Stat a) a
type ExprF a = Ann (Expr a) a
type TypeF a = Ann (Type a) a
type IdentF a = Ann (Ident a) a
type AssignRHSF a = Ann (AssignRHS a) a
type AssignLHSF a = Ann (AssignLHS a) a
type ArrayElemF a = Ann (ArrayElem a) a
type PairElemF a = Ann (PairElem a) a
type LiterF a = Ann (Liter a) a
{-
type ProgramF a = Cofree Program a
type FuncF a = Cofree Func a
type ParamF a = Cofree Param a
type StatF a = Cofree Stat a
type ExprF a = Cofree Expr a
type TypeF a = Cofree Type a
type IdentF a = Cofree Ident a
-}
{-
instance Functor Pass where
  fmap _ Pass = Pass

instance Functor Program where
  fmap f (Program fs s) = Program (map (fmap f) fs) (fmap f s)

instance Functor Func where
  fmap f (Func t i ps s) = 
    Func (fmap f t) (fmap f i) (map (fmap f) ps) (fmap f s)

instance Functor Param where
  fmap f (Param t i) = Param (fmap f t) (fmap f i)

instance Functor Stat where
  fmap _ Skip = Skip
  fmap f (Exit e) = Exit (fmap f e)

instance Functor Expr where
  fmap _ (IntLiter i) = IntLiter i
  fmap _ (BoolLiter b) = BoolLiter b 
  fmap _ (CharLiter c) = CharLiter c
  fmap _ (StringLiter s) = StringLiter s

instance Functor Type where
  fmap _ TInt = TInt
  fmap _ TBool = TBool
  fmap _ TChar = TChar
  fmap _ TStr = TStr
  fmap f (TArray t) = TArray (fmap f t)
  fmap f (TPair t1 t2) = TPair (fmap f t1) (fmap f t2)
  fmap _ Any = Any

instance Functor Ident where
  fmap _ (Ident s) = Ident s
-}
