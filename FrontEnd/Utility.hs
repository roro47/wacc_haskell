module FrontEnd.Utility where

import FrontEnd.Parser
import FrontEnd.Lexer
import FrontEnd.AST
import Data.Char
import Data.List

visualizeAST :: String -> IO([String])
visualizeAST file = fmap (index.addline.showProgramF) ast
  where
    ast = parseFile file

printAST :: String -> IO(IO ())
printAST file = do
  ast <- visualizeAST file
  sequence (fmap putStrLn ast)
  return (putStrLn "") -- eliminate unecessary print of state

addline :: [String] -> [String]
addline (a:as)
  = a : (map (\x -> let (space, content) = span (== ' ') x in space ++ "- " ++ content) as)

index :: [String] -> [String]
index = (zipWith (++) (map show [0 ..])).(map ("    "++))

indent :: [String] -> [String]
indent = map (" " ++)

removeAnn :: (Ann f) -> f
removeAnn (Ann f (_,_))= f

strip :: String -> String
strip = filter (not.isSpace)

showStatListF :: StatListF a -> [String]
showStatListF = showStatList.removeAnn

showStatList :: StatList a -> [String]
showStatList (StatList sl)= concatMap showStatF sl

showProgramF :: ProgramF a -> [String]
showProgramF = showProgram.removeAnn

showProgram :: Program a -> [String]
showProgram (Program f s) = "Program" : (indent rest)
  where
    rest = (concatMap showFuncF f) ++ ["int main()"] ++ (showStatListF s)

showFuncF :: FuncF a -> [String]
showFuncF = showFunc.removeAnn

showFunc :: Func a -> [String]
showFunc (Func ty identifier params statList)
  = (ty' ++ " " ++ identifier' ++ "(" ++ params' ++ ")") : (indent statList')
    where
      ty' = concat $ showTypeF ty
      identifier' = showIdentF identifier
      params' = intercalate ", " $ map showParamF params
      statList' = showStatListF statList

showTypeF :: TypeF a -> [String]
showTypeF = showType.removeAnn

showType :: Type a -> [String]
showType (TInt) = ["int"]
showType (TBool) = ["bool"]
showType (TChar) = ["char"]
showType (TStr) = ["string"]
showType (TArray a) = [strip (concat $ showTypeF a) ++ "[]"]
showType (TPair a b) = [strip (f ++ s)]
  where
    f = concat $ showTypeF a
    s = concat $ showTypeF b
showType _ = [""] -- any. Dont need to print out

showIdentF :: IdentF a -> String
showIdentF = showIdent.removeAnn

showIdent :: Ident a -> String
showIdent (Ident str) = str

showParamF :: ParamF a -> String
showParamF = showParam.removeAnn

showParam :: Param a -> String
showParam (Param ty ident) = ty' ++ " " ++ ident'
  where
    ty' = concat $ showTypeF ty
    ident' = showIdentF ident


showStatF :: (StatF a) -> [String]
showStatF = showStat.removeAnn

showStat :: (Stat a) -> [String]
showStat Skip = ["SKIP"]
showStat (Declare ty ident rhs) =  "DECLARE" : (indent rest)
  where
    rest = "TYPE" : ty' : "LHS" : ident' : "RHS" : rhs'
    ty' = concat (indent.showTypeF $ ty)
    ident' = " " ++ (showIdentF ident)
    rhs' = indent $ (showAssignRHSF rhs)
showStat (Assign lhs rhs) = "ASSIGNMENT" : (indent rest)
  where
    rest = "LHS" : lhs' ++ ["RHS"] ++ rhs'
    lhs' = indent $ showAssignLHSF lhs
    rhs' = indent $ showAssignRHSF rhs
showStat (Read lhs) = "READ" : (indent $ showAssignLHSF lhs)
showStat (Free expr) = "FREE" : (indent $ showExprF expr)
showStat (Return expr) = "RETURN" : (indent $ showExprF expr)
showStat (Exit expr) = "EXIT" : (indent $ showExprF expr)
showStat (Print expr) = "PRINT" : (indent $ showExprF expr)
showStat (Println expr) = "PRINTLN" : (indent $ showExprF expr)
showStat (If expr statList1 statList2) = "IF" : (indent rest)
  where
    rest = "CONDITION" : expr' ++ ["THEN"] ++ stat1' ++ ["ELSE"] ++ stat2'
    expr' = (indent $ showExprF expr)
    stat1' = indent $ showStatListF statList1
    stat2' = indent $ showStatListF statList2
showStat (While expr statList) = "LOOP" : (indent rest)
  where
    rest = "CONDITION" : expr' ++["DO"] ++ stat'
    expr' = (indent $ showExprF expr)
    stat' = indent $ showStatListF statList
showStat (Subroutine program) = showStatListF program

showAssignLHSF :: AssignLHSF a -> [String]
showAssignLHSF = showAssignLHS.removeAnn

showAssignLHS :: AssignLHS a -> [String]
showAssignLHS (IdentLHS ident) = [showIdentF ident]
showAssignLHS (ArrayElemLHS array) = showArrayEF array
showAssignLHS (PairElemLHS paire) = showPairEF paire

showPairEF :: PairElemF a -> [String]
showPairEF = showPairE.removeAnn

showPairE :: PairElem a -> [String]
showPairE (PairElemFst expr) = "FST" : indent (showExprF expr)
showPairE (PairElemSnd expr) = "SND" : indent (showExprF expr)

showArrayEF :: ArrayElemF a -> [String]
showArrayEF = showArrayE.removeAnn

showArrayE :: ArrayElem a -> [String]
showArrayE (ArrayElem ident exprs) = (showIdentF ident) : (indent rest)
  where
    rest = "[]" : (indent (concatMap showExprF exprs))

showAssignRHSF :: AssignRHSF a -> [String]
showAssignRHSF = showAssignRHS.removeAnn

showAssignRHS :: AssignRHS a -> [String]
showAssignRHS (ExprRHS expr) = showExprF expr
showAssignRHS (NewPair expr1 expr2) = "NEW_PAIR" : (indent rest)
  where
    rest = "FST" : (indent $ showExprF expr1) ++ ["SND"] ++ (indent $ showExprF expr2)
showAssignRHS (PairElemRHS pe) = showPairEF pe
showAssignRHS (Call ident exprs) = (showIdentF ident): (concatMap showExprF exprs)
showAssignRHS (ArrayLiter exprs) = "ARRAY LITERAL" : (indent $ concatMap showExprF exprs)

showExprF :: ExprF a -> [String]
showExprF = showExpr.removeAnn

showExpr :: Expr a -> [String]
showExpr (LiterExpr l) = showLiter (removeAnn l)
showExpr (IdentExpr ident) = [showIdentF ident]
showExpr (ArrayExpr array) = showArrayEF array
showExpr (UExpr Neg (Ann (LiterExpr(Ann (IntLiter i) _)) _ )) = ['-':(show i)]
showExpr (UExpr uOp expr) = showUop(uOp) : (indent $ showExprF expr)
showExpr (BExpr bOp exp1 exp2) = showBop(bOp) : (indent $ ((showExprF exp1) ++ (showExprF exp2)))
showExpr (BracketExpr expr) = showExprF expr

showLiter :: Liter a -> [String]
showLiter (IntLiter i) = [show i]
showLiter (BoolLiter b) = [map toLower (show b)]
showLiter (CharLiter c) = [show c]
showLiter (StringLiter s) = [show s]
showLiter Null = ["null"]
showLiter PairLiter = ["ppppppppair"]

showBop :: BinaryOp -> String
showBop Mul = "*"
showBop Div = "/"
showBop Mod = "%"
showBop Plus = "+"
showBop Minus = "-"
showBop G = ">"
showBop GEq = ">="
showBop L = "<"
showBop LEq = "<="
showBop Eq = "=="
showBop NEq = "!="
showBop And = "&&"
showBop Or = "||"

showUop :: UnaryOp -> String
showUop Not = "~" --not sure
showUop Neg = "-"
showUop Pos = "!"
showUop Len = "len"
showUop Ord = "ord"
showUop Chr = "chr"
