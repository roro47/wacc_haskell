module FrontEnd.AST where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

import Text.ParserCombinators.Parsec.Pos

import FrontEnd.Lexer

data Pass c = Pass 

data Program a = Program [FuncF a] (StatListF a) deriving (Eq, Show)

data Func a = Func (TypeF a) (IdentF a) [ParamF a] (StatListF a) 
                deriving (Eq, Show)
              

data Param a = Param (TypeF a) (IdentF a) deriving (Eq, Show)

data StatList a = StatList [StatF a] deriving (Eq, Show)

data Stat a = Skip
            | Declare (TypeF a) (IdentF a) (AssignRHSF a)
            | Assign (AssignLHSF a) (AssignRHSF a)
            | Read (AssignLHSF a)
            | Free (ExprF a)
            | Return (ExprF a)
            | Exit (ExprF a) 
            | Print (ExprF a)
            | Println (ExprF a)
            | If (ExprF a) (StatListF a) (StatListF a)
            | While (ExprF a) (StatListF a)
            | Subroutine (StatListF a)
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
            | None
            | TFunc (TypeF a) [TypeF a] -- return type, param type
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

data Ann f = Ann f (SourcePos, Type ()) deriving Show

data Parse = Parse deriving (Show, Eq)
data Semantic = Semantic deriving (Show, Eq)

type ProgramF a = Ann  (Program a) 
type FuncF a = Ann  (Func a)
type ParamF a = Ann  (Param a)
type StatF a = Ann  (Stat a)
type ExprF a = Ann  (Expr a)
type TypeF a = Ann  (Type a)
type IdentF a = Ann  (Ident a)
type AssignRHSF a = Ann  (AssignRHS a)
type AssignLHSF a = Ann  (AssignLHS a)
type ArrayElemF a = Ann  (ArrayElem a)
type PairElemF a = Ann  (PairElem a)
type LiterF a = Ann  (Liter a)
type StatListF a = Ann (StatList a)

instance (Eq f) => Eq (Ann f) where
  (Ann f1 _) == (Ann f2 _) = f1 == f2


