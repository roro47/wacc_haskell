module FrontEnd.SemanticAnalyzer where

import Prelude

import Data.List hiding (insert)
import Control.Applicative
import Control.Monad.State
import Control.Monad.Except
import Data.HashMap as HashMap hiding (map)
import Text.ParserCombinators.Parsec hiding ((<|>))
import Text.ParserCombinators.Parsec.Pos

import FrontEnd.AST
import FrontEnd.Parser

data SemanticError = SemanticError String deriving (Show, Eq)

type Symbol = String

type SymbolT = Map String (TypeF ())  -- declaration is stored at type
type SymbolTs = [SymbolT]
type Analyzer = StateT SymbolTs (Either String)


-- dummy type
dummyAnn = (newPos "d" 1 2, None)
anyType = (Ann Any dummyAnn)
pairType = TPair anyType anyType
arrayType = TArray anyType


line = \e -> show $ sourceLine $ errorPos e
col = \e -> show $ sourceLine $ errorPos e
name = \e -> show $ sourceName $ errorPos e

getT (Ann _ (pos, t)) = t

unwrap (Ann x _) = x

isArray (TArray _) = True
isArray _ = False

getSymbol :: IdentF () -> String
getSymbol (Ann (Ident s) _) = s


showId :: IdentF () -> String
showId (Ann (Ident id) _) = id


declaredErr symbol pos1 pos2
 = "symbol " ++ symbol ++ show pos1 ++ " already declared at "  ++ show pos2


typeErr msg pos expected actual =
  "type not matched at " ++ show pos ++ "\n" ++
  msg' ++
  "expected : " ++ (concat $ intersperse "," (map show expected)) ++ "\n" ++
  "actual: " ++ (concat $ intersperse "," (map show actual)) ++ "\n"
  where msg' = if msg /= "" then msg ++ "\n" else msg

pushScope :: Analyzer ()
pushScope = do
           tables <- get
           put (HashMap.empty:tables)

-- pre : stack of scope must be not empty
popScope :: Analyzer ()
popScope = do
           tables <- get
           put $ tail tables
           

addSymbol :: IdentF () -> TypeF () -> Analyzer ()
addSymbol id t = do
                     (table:tables) <- get
                     put ((insert symbol t table):tables)
  where Ann (Ident symbol) _ = id
                     
lookUpSymbol :: IdentF () -> Analyzer (Maybe (TypeF ()))
lookUpSymbol id =
  do
    tables <- get
    return $ foldl (<|>) Nothing $ map (HashMap.lookup symbol) tables
  where Ann (Ident symbol) _ = id


analyzeProgramF :: ProgramF () -> Analyzer (ProgramF ())
analyzeProgramF p@(Ann (Program fs stat) ann@(pos, none)) =
  do
    pushScope
    fs' <- mapM analyzeFuncF fs
    stat' <- analyzeStatListF stat
    popScope
    return $ Ann (Program fs' stat') ann 

analyzeFuncF :: FuncF () -> Analyzer (FuncF ())
analyzeFuncF f@(Ann (Func t symbol ps stats) (pos, none)) =
    lookUpSymbol symbol >>= \maybeT ->
      case maybeT of
        Just (Ann _ (pos', _)) -> throwError $ "already declared" 
        otherwise ->
          pushScope >>= \_ ->
          mapM (\(Ann (Param t pName) _) -> addSymbol pName t) ps >>= \_ ->
          analyzeStatListF stats >>= \stats' ->
          popScope >>= \_ ->
          addSymbol symbol (Ann (TFunc t paramTs) (pos, none)) >>= \_ ->
          return f
   where paramTs = map (\(Ann (Param t _) _) -> t) ps

                   
analyzeStatListF :: StatListF () -> Analyzer (StatListF ())
analyzeStatListF (Ann (StatList stats) ann) =
  mapM analyzeStatF stats >>= \stats' ->
  return $ Ann (StatList stats') ann

typeCheckExpr :: String -> Type () -> ExprF () -> Analyzer (ExprF ())
typeCheckExpr err t e@(Ann expr (posExpr, exprT))
   = if t /= exprT
     then throwError $ typeErr err posExpr [t] [exprT]
     else return e


typeCheckArray :: String -> TypeF () -> AssignRHSF () -> Analyzer (AssignRHSF ())
typeCheckArray msg (Ann t _) rhs@(Ann (ExprRHS expr) _) =
  typeCheckExpr msg t expr >>= \_ ->
  return rhs
  
typeCheckArray msg (Ann t _) rhs@(Ann (ArrayLiter exprs) _) =
  mapM (typeCheckExpr msg arrayT) exprs >>= \_ ->
  return rhs
  where (TArray (Ann arrayT _)) = t

analyzeStatF :: StatF () -> Analyzer (StatF ())

analyzeStatF s@(Ann Skip _) = return $ s

analyzeStatF (Ann s@(Declare t symbol rhs) (pos, none)) =
  lookUpSymbol symbol >>= \maybeT ->
    case maybeT of
      Just (Ann _ (pos', _)) -> throwError $ "symbol " ++ showId symbol ++ show pos ++  " already declared at " ++ show pos'
      otherwise -> analyzeAssignRHSF rhs >>= \rhs'@(Ann _ (pos, rhsT)) ->
                   if isArray declareT
                   then typeCheckArray err t rhs' >>= \_ ->
                        addSymbol symbol t >>= \_ ->
                        return $ Ann (Declare t symbol rhs') (pos, none)
                   else  if rhsT /= declareT
                         then throwError $ typeErr err pos [declareT] [rhsT]
                         else addSymbol symbol t >>= \_ ->
                              return $ Ann (Declare t symbol rhs') (pos, none)
   where (Ann declareT _ ) = t
         err = "Assign wrong type of value in declaration"
         getExprs (ArrayLiter exprs) = exprs

analyzeStatF (Ann (Assign lhs rhs) ann@(pos, none)) =
    analyzeAssignLHSF lhs >>= \ lhs'@(Ann _ (pos1, lT)) ->
    analyzeAssignRHSF rhs >>= \ rhs'@(Ann expr (pos2, rT)) ->
    case lT of
      TArray (Ann t _) -> case rT of
                    TArray _ -> mapM_ (typeCheckExpr errArray t) (getExprs expr) >>= \_ ->
                                return $ Ann (Assign lhs' (Ann expr (pos2, lT))) ann
                    otherwise -> throwError $ typeErr err pos2 [lT] [rT] 
      otherwise -> if lT == rT
                   then return $ Ann (Assign lhs' rhs') (pos, none)
                   else throwError $ typeErr err pos2 [lT] [rT]
  where err = "Assign wrong type of value"
        errArray = "Wrong type of value in array"
        getExprs (ArrayLiter exprs) = exprs
        

analyzeStatF (Ann (Read lhs) (pos, _)) =
    analyzeAssignLHSF lhs >>= \lhs'@(Ann _ (posLHS, tl)) ->
    if tl /= TInt && tl /= TChar
    then throwError $ typeErr err posLHS [TInt, TChar] [tl]
    else return $ Ann (Read lhs') (pos, tl)
  where err = "Support reading int and char only"

analyzeStatF (Ann (Free expr) ann@(pos, none)) =
  analyzeExprF expr >>= \expr'@(Ann _ (posExpr, t)) ->
  case t of
    TArray _ -> return $ Ann (Free expr') ann
    TPair _ _ -> return $ Ann (Free expr') ann
    otherwise -> throwError $ typeErr err posExpr [TArray any, TPair any any] [t] 
  where any = Ann Any ann
        err = "Support freeing array and pair only"

analyzeStatF (Ann (Return expr) ann@(pos, none)) =
  analyzeExprF expr >>= \expr'@(Ann _ (_, t)) ->
  return $ Ann (Return expr') (pos, t) -- tag type on this statement for
                                       -- function return type check

analyzeStatF (Ann (Exit expr) ann@(pos, none)) =
  analyzeExprF expr >>= \expr'@(Ann _ (posExpr, t)) ->
  if t /= TInt
  then throwError $ typeErr err posExpr [TInt] [t]
  else return $ Ann (Exit expr') ann
  where err = "Exit value not of type int"

analyzeStatF (Ann (Print expr) ann@(pos, none)) =
  analyzeExprF expr >>= \expr' ->
  return $ Ann (Print expr') ann

analyzeStatF (Ann (Println expr) ann@(pos, none)) =
  analyzeExprF expr >>= \expr' ->
  return $ Ann (Println expr') ann

analyzeStatF (Ann (If expr stat1 stat2) ann@(pos, none)) =
  analyzeExprF expr >>= \expr'@(Ann _ (posExpr, t)) ->
  if t /= TBool
  then throwError $ typeErr err posExpr [TBool] [t]
  else analyzeStatListF stat1 >>= \stat1' ->
       analyzeStatListF stat2 >>= \stat2' ->
       return $ Ann (If expr' stat1' stat2') ann
  where err = "Condition must be of type bool"

analyzeStatF (Ann (While expr stat) ann@(pos, none)) =
  analyzeExprF expr >>= \expr'@(Ann _ (posExpr, t)) ->
  if t /= TBool
  then throwError $ typeErr err posExpr [TBool] [t]
  else analyzeStatListF stat >>= \stat' ->
       return $ Ann (While expr' stat') ann
  where err = "Condition must be of type bool"
  
analyzeStatF (Ann (Subroutine stat) ann) =
  analyzeStatListF stat >>= \stat' ->
  return $ Ann (Subroutine stat') ann 

analyzeAssignLHSF :: AssignLHSF () -> Analyzer (AssignLHSF ())
analyzeAssignLHSF (Ann lhs@(IdentLHS symbol) (pos, _)) =
    lookUpSymbol symbol >>= \maybeT ->
      case maybeT of
        Just (Ann t _) -> return $ Ann lhs (pos, t)
        otherwise -> throwError $ "Assign to undeclared symbol " ++ show symbol
                                  ++ " at " ++ show pos ++ "\n"

analyzeAssignLHSF (Ann (ArrayElemLHS a) (pos, _)) =
  analyzeArrayElemF a >>= \a'@(Ann _ (_, t)) ->
  return $ Ann (ArrayElemLHS a') (pos, t)


analyzeAssignLHSF (Ann (PairElemLHS p) (pos, _)) =
  analyzePairElemF p >>= \p'@(Ann _ (_, t)) ->
  return $ Ann (PairElemLHS p') (pos, t)

analyzePairElemF :: PairElemF () -> Analyzer (PairElemF ())
analyzePairElemF (Ann (PairElemFst expr) (pos, _)) =
  analyzeExprF expr >>= \expr'@(Ann _ (_, t)) ->
  case t of
    TPair t1 _ -> return $ Ann (PairElemFst expr') (pos, unwrap t1)
    otherwise -> throwError $ "expected pair"

analyzePairElemF (Ann (PairElemSnd expr) (pos, _)) =
  analyzeExprF expr >>= \expr'@(Ann _ (_, t)) ->
  case t of
    TPair _ t2 -> return $ Ann (PairElemSnd expr') (pos, unwrap t2)
    otherwise -> throwError $ "expected pair"
  
analyzeArrayElemF :: ArrayElemF () -> Analyzer (ArrayElemF ())
analyzeArrayElemF (Ann e@(ArrayElem symbol exprs) (pos, _)) =
  lookUpSymbol symbol >>= \maybeT ->
    case maybeT of
      Nothing -> throwError "array not declared"
      Just (Ann t _) ->
        mapM analyzeExprF exprs >>= \exprs' ->
        case findIndex (t /=) (map getT exprs') of
          Just i -> throwError "some expr has incorrect type"
          Nothing -> return $ Ann (ArrayElem symbol exprs') (pos, t)

analyzeAssignRHSF :: AssignRHSF () -> Analyzer (AssignRHSF ())
analyzeAssignRHSF (Ann (ExprRHS expr) (pos, _)) =
  do
    expr' <- analyzeExprF expr
    return $ Ann (ExprRHS expr') (pos, getT expr')

analyzeAssignRHSF (Ann (NewPair expr1 expr2) (pos, _)) =
  do
    e1@(Ann _ (pos1, t1)) <- analyzeExprF expr1
    e2@(Ann _ (pos2, t2)) <- analyzeExprF expr2
    return $ Ann (NewPair e1 e2)
            (pos, TPair (Ann t1 (pos, None)) (Ann t2 (pos, None)))

analyzeAssignRHSF (Ann (PairElemRHS p) (pos, _)) =
  analyzePairElemF p >>= \p'@(Ann _ (_, t)) ->
  return $ Ann (PairElemRHS p') (pos, t)

analyzeAssignRHSF (Ann (Call symbol exprs) (pos, _)) =
  lookUpSymbol symbol >>= \maybeT ->
  case maybeT of
    Nothing -> throwError $ "Function symbol " ++ show symbol ++ " at " ++ show pos
                            ++ " not found"
    Just (Ann t _) -> 
      case t of
        TFunc (Ann tOut _) tIns ->
          mapM analyzeExprF exprs >>= \exprs' ->
          if length tIns /= length exprs'
          then throwError ("For function call " ++ show symbol ++ " at " ++ show pos
                           ++", require " ++ show (length tIns) ++ " parameters," ++
                           show (length exprs') ++ " are given.\n")
          else mapM (\(t1, e) -> match t1 e) (zip tIns exprs') >>= \_ ->
               return $ Ann (Call symbol exprs') (pos, tOut)
        otherwise -> throwError $ "Symbol " ++ show symbol ++ " at " ++ show pos ++
                                  " is not a function symbol"
   where match :: TypeF () -> ExprF () -> Analyzer (ExprF ())
         match (Ann t1 _) expr@(Ann _ (pos, t2)) =
           if t1 == t2 then return expr
           else throwError $ typeErr "Function paramater type mismatched" pos [t1] [t2]

-- TODO: fix list of expressions' print out message
analyzeAssignRHSF (Ann (ArrayLiter []) (pos, _)) =
  return $ Ann (ArrayLiter []) (pos, TArray (Ann Any (pos, None)))

-- won't check that all the expressions in the array would have the same type
analyzeAssignRHSF (Ann (ArrayLiter exprs) (pos, _)) = 
  mapM analyzeExprF exprs >>= \exprs' ->
  return $ Ann (ArrayLiter exprs') (pos, TArray (Ann Any (pos, None)))

analyzeExprF :: ExprF () -> Analyzer (ExprF ())
analyzeExprF (Ann (LiterExpr i) (pos, _)) =
  do
    liter <- analyzeLiterF i
    return $ Ann (LiterExpr liter) (pos, getT liter) 

analyzeExprF (Ann e@(IdentExpr symbol) (pos, _)) =
  lookUpSymbol symbol >>= \maybeT ->
    case maybeT of
      Nothing -> throwError "symbol not found"
      Just (Ann t _) -> return $ Ann e (pos, t)

analyzeExprF (Ann (ArrayExpr a) (pos, _)) =
  analyzeArrayElemF a >>= \a'@(Ann _ (_, t)) ->
  return $ Ann (ArrayExpr a') (pos, t)
        
analyzeExprF (Ann (UExpr op e) (pos, _)) =
  analyzeExprF e >>= \e'@(Ann _ (_, t)) ->
  return $ Ann (UExpr op e') (pos, t)

analyzeExprF (Ann (BExpr bop e1 e2) (pos, _)) =
  analyzeExprF e1 >>= \e1'@(Ann _ (_, t1)) ->
  analyzeExprF e2 >>= \e2'@(Ann _ (_, t2)) ->
  case bop of
    Mul -> match2 bop TInt [TInt] e1' e2'
    Div -> match2 bop TInt [TInt] e1' e2'
    Mod -> match2 bop TInt [TInt] e1' e2'
    Plus -> match2 bop TInt [TInt] e1' e2'
    Minus -> match2 bop TInt [TInt] e1' e2'
    G -> match2 bop TBool [TInt, TChar] e1' e2'
    GEq -> match2 bop TBool [TInt, TChar] e1' e2'
    L -> match2 bop TBool [TInt, TChar] e1' e2'
    LEq -> match2 bop TBool [TInt, TChar] e1' e2'
    Eq -> match2 bop TBool [TInt, TChar, TBool, pairType, arrayType] e1' e2'
    NEq -> match2 bop TBool [TInt, TChar, TBool, pairType, arrayType] e1' e2'
    And -> match2 bop TBool [TBool] e1' e2'
    Or -> match2 bop TBool [TBool] e1' e2'
  where
    match1 :: [Type ()] -> ExprF () -> Analyzer (ExprF ())
    match1 ts e@(Ann expr (pos, exprT))
      = if elem exprT ts
        then return e
        else throwError $ typeErr "" pos ts [exprT]
    match2 :: BinaryOp -> Type () -> [Type ()] -> ExprF () ->
              ExprF () -> Analyzer (ExprF ())
    match2 bop returnT ts e1@(Ann expr1 (pos1, expr1T)) e2@(Ann expr2 (pos2, expr2T))
      = match1 ts e1 >>= \_ ->
        match1 ts e2 >>= \_ ->
        if expr1T /= expr2T
        then throwError $ typeErr "" pos2 [expr1T] [expr2T]
        else return $ Ann (BExpr bop e1 e2) (pos1, returnT)


analyzeExprF (Ann (BracketExpr expr) (pos, _)) =
  analyzeExprF expr >>= \expr'@(Ann _ (_, t)) ->
  return $ Ann (BracketExpr expr') (pos, t)

analyzeLiterF :: LiterF () -> Analyzer (LiterF ())
analyzeLiterF (Ann liter (pos, _)) =
  do case liter of
       IntLiter _ -> return $ Ann liter (pos, TInt)
       BoolLiter _ -> return $ Ann liter (pos, TBool) 
       CharLiter _ -> return $ Ann liter (pos, TChar)
       StringLiter _ -> return $ Ann liter (pos, TStr)
       otherwise -> return $ Ann liter (pos, Any)

analyzeFile :: String -> IO (ProgramF ())
analyzeFile file =
  do
    program <- readFile file
    case parse parseProgramF "" program of
       Left e -> print e >>
                fail ("parse error: " ++ "at line " ++ line e ++ " and col " ++ col e ++ " with file " ++ file)
       Right p -> case evalStateT (analyzeProgramF p) [] of
                    Left e -> putStr e >> fail ""
                    Right p' -> return p'
  where line = \e -> show $ sourceLine $ errorPos e
        col = \e -> show $ sourceLine $ errorPos e
        name = \e -> show $ sourceName $ errorPos e


