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
          | TFunc [Type] [Type] Type -- stored in scope after func declaration
          | Void
          | None
          | T -- similar to type parameter

data Expr a = IntLiter Int
            | BoolLiter Bool
            | CharLiter Char
            | StringLiter String
            | IdentExpr (IdentF a)
            | Null
            | BracketExpr (ExprF a)
            | ArrayElem (IdentF a) [ExprF a]
            | ArrayLiter [ExprF a]
            | FuncExpr (FuncAppF a)
            deriving (Eq, Show)

{- The datatype for all functions, including built-in and user decleared ones.
   The IdentF represents the name of the function and the exprf are the parameters. -}
data FuncApp a = FuncApp Type (IdentF a) [ExprF a] deriving (Eq, Show)

data Ident a = Ident String deriving (Eq)

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


arrayT = TArray TAny
pairT = TPair TAny TAny

isTFunc :: Type -> Bool
isTFunc (TFunc _ _ _) = True
isTFunc _ = False


{- Table of built in functions. Handled in parseFuncAppStat
   User defined functions are without allowed type -}
builtInFunc :: [(String, Type)]
builtInFunc =
-- name        allowed type            parameters     return type
  [("skip",    TFunc []                []             Void),
   ("read",    TFunc [TInt, TChar]     [T]            Void),
   ("free",    TFunc [arrayT, pairT]   [T]            Void),
   ("print",   TFunc [TAny]            [T]            Void),
   ("println", TFunc [TAny]            [T]            Void),
   ("newpair", TFunc []                [TAny, TAny]   pairT),
   ("fst",     TFunc [TPair TAny TAny] [TPair T TAny] T),
   ("snd",     TFunc [TPair TAny TAny] [TPair TAny T] T),
   ("!",       TFunc []                [TBool]        TBool),
   ("#pos",     TFunc []               [TInt]         TInt),
   ("#neg",     TFunc []               [TInt]         TInt),
   ("len",     TFunc []               [TArray TAny]  TInt),
   ("ord",     TFunc []               [TChar]        TInt),
   ("chr",     TFunc []               [TInt]         TChar),
   ("*",       TFunc []               [TInt, TInt]   TInt),
   ("/",       TFunc []               [TInt, TInt]   TInt),
   ("%",       TFunc []               [TInt, TInt]   TInt),
   ("+",       TFunc []               [TInt, TInt]   TInt),
   ("-",       TFunc []               [TInt, TInt]   TInt),
   (">",       TFunc [TInt, TChar]    [T, T]         TBool),
   (">=",      TFunc [TInt, TChar]    [T, T]         TBool),
   ("<",       TFunc [TInt, TChar]    [T, T]         TBool),
   ("<=",      TFunc [TInt, TChar]    [T, T]         TBool),
   ("==",      TFunc [TAny]           [T, T]         TBool),
   ("!=",      TFunc [TAny]           [T, T]         TBool),
   ("&&",      TFunc []               [TBool, TBool] TBool),
   ("||",      TFunc []               [TBool, TBool] TBool)]


instance (Show f) => Show (Ann f) where
  show (Ann f _) = show f

instance (Eq f) => Eq (Ann f) where
  (Ann f1 _) == (Ann f2 _) = f1 == f2

instance Eq (Type) where
  TInt == TInt = True
  TBool == TBool = True
  TChar == TChar = True
  TStr == TStr = True
  T == T = True
  None == None = True
  Void == Void = True
  TAny == _ = True
  _ == TAny = True
  (TArray TChar) == TStr = True
  TStr == (TArray TChar) = True
  (TArray t1) == (TArray t2) = t1 == t2
  (TPair ft1 st1) == (TPair ft2 st2) = (ft1 == ft2) && (st1 == st2)
  (TFunc aT1 pT1 rT1) == (TFunc aT2 pT2 rT2) =
    (aT1 == aT2) && (pT1 == pT2) && (rT1 == rT2)
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
  show (TFunc _ ts t) = "TFunc (" ++ show t ++ ") " ++
    "(" ++ intersperse ',' (concat $ map show ts) ++ ")"
  show None = ""
  show T = "T"

instance Show (Ident a) where
  show (Ident s) = show s

typeLen :: Type -> Int
typeLen TChar = 1
typeLen TBool = 1
typeLen _ = 4
