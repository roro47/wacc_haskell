module FrontEnd.SemanticAnalyzer where

import Prelude

import Data.List hiding (insert)
import Control.Applicative
import Control.Monad.State
import Control.Monad.Except
import Data.HashMap as HashMap hiding (map)
import Text.ParserCombinators.Parsec.Pos

import FrontEnd.AST
import FrontEnd.Parser

data SemanticError = SemanticError String deriving (Show, Eq)

type Symbol = String

type SymbolT = Map String (TypeF ())  -- declaration is stored at type
type SymbolTs = [SymbolT]
type Analyzer = StateT SymbolTs (Either String)


getT (Ann _ (pos, t)) = t

unwrap (Ann x _) = x

getSymbol :: IdentF () -> String
getSymbol (Ann (Ident s) _) = s


showId :: IdentF () -> String
showId (Ann (Ident id) _) = id


declaredErr symbol pos1 pos2
 = "symbol " ++ symbol ++ show pos1 ++ " already declared at "  ++ show pos2

  

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
analyzeProgramF p@(Ann (Program fs stat) _) =
  do
    pushScope
    mapM analyzeFuncF fs
    analyzeStatListF stat
    popScope
    return p

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

analyzeStatF :: StatF () -> Analyzer (StatF ())
analyzeStatF s@(Ann Skip _) = return $ s
analyzeStatF (Ann s@(Declare t symbol rhs) (pos, none)) =
  lookUpSymbol symbol >>= \maybeT ->
    case maybeT of
      Just (Ann t (pos', _)) -> throwError $ "symbol " ++ showId symbol ++ show pos ++  " already declared at " ++ show pos'
      otherwise -> analyzeAssignRHSF rhs >>= \rhs' ->
                   addSymbol symbol t >>= \_ ->
                   return $ Ann (Declare t symbol rhs') (pos, none)

analyzeStatF (Ann (Assign lhs rhs) (pos, none)) =
  analyzeAssignLHSF lhs >>= \ lhs'@(Ann _ (pos, tl)) ->
  analyzeAssignRHSF rhs >>= \ rhs'@(Ann _ (pos, tr)) ->
  if tl == tr
  then return $ Ann (Assign lhs' rhs') (pos, none)
  else throwError "mismatched type" 
  
analyzeStatF (Ann (Read lhs) (pos, _)) =
  analyzeAssignLHSF lhs >>= \lhs'@(Ann _ (_, tl)) ->
  if tl /= TInt && tl /= TChar
  then throwError "cannot read type other than TInt and TChar"
  else return $ Ann (Read lhs') (pos, tl)


analyzeStatF (Ann (Free expr) ann@(pos, none)) =
  analyzeExprF expr >>= \expr'@(Ann _ (_, t)) ->
  case t of
    TArray _ -> return $ Ann (Free expr') ann
    TPair _ _ -> return $ Ann (Free expr') ann
    otherwise -> throwError $ "cannot free type other than TArray, TPair"


analyzeStatF (Ann (Return expr) ann@(pos, none)) =
  analyzeExprF expr >>= \expr'@(Ann _ (_, t)) ->
  return $ Ann (Return expr') ann


analyzeStatF (Ann (Exit expr) ann@(pos, none)) =
  analyzeExprF expr >>= \expr'@(Ann _ (_, t)) ->
  if t /= TInt
  then throwError "exit value not of type int"
  else return $ Ann (Exit expr') ann

analyzeStatF (Ann (Print expr) ann@(pos, none)) =
  analyzeExprF expr >>= \expr' ->
  return $ Ann (Print expr') ann

analyzeStatF (Ann (Println expr) ann@(pos, none)) =
  analyzeExprF expr >>= \expr' ->
  return $ Ann (Println expr') ann

analyzeStatF (Ann (If expr stat1 stat2) ann@(pos, none)) =
  analyzeExprF expr >>= \expr'@(Ann _ (_, t)) ->
  if t /= TBool
  then throwError "condition type not bool"
  else analyzeStatListF stat1 >>= \stat1' ->
       analyzeStatListF stat2 >>= \stat2' ->
       return $ Ann (If expr' stat1' stat2') ann

analyzeStatF (Ann (While expr stat) ann@(pos, none)) =
  analyzeExprF expr >>= \expr'@(Ann _ (_, t)) ->
  if t /= TBool
  then throwError "condition type not bool"
  else analyzeStatListF stat >>= \stat' ->
       return $ Ann (While expr' stat') ann

analyzeStatF (Ann (Subroutine stat) ann) =
  analyzeStatListF stat >>= \stat' ->
  return $ Ann (Subroutine stat') ann 

analyzeAssignLHSF :: AssignLHSF () -> Analyzer (AssignLHSF ())
analyzeAssignLHSF (Ann lhs@(IdentLHS symbol) (pos, _)) =
    lookUpSymbol symbol >>= \maybeT ->
      case maybeT of
        Just (Ann t _) -> return $ Ann lhs (pos, t)
        otherwise -> throwError $ "symbol " ++ showId symbol ++ show pos ++ " not defined"

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
    return $ Ann (ExprRHS expr) (pos, getT expr')

analyzeAssignRHSF (Ann (NewPair expr1 expr2) (pos, _)) =
  do
    e1@(Ann _ (pos1, t1)) <- analyzeExprF expr1
    e2@(Ann _ (pos2, t2)) <- analyzeExprF expr2
    return $ Ann (NewPair e1 e2)
            (pos, TPair (Ann t1 (pos, None)) (Ann t2 (pos, None)))

analyzeAssignRHSF (Ann (PairElemRHS p) (pos, _)) =
  analyzePairElemF p >>= \p'@(Ann _ (_, t)) ->
  return $ Ann (PairElemRHS p') (pos, t)


-- Todo: figure out error message once you decide how to print the type
-- how to check a list of expressions nicely
analyzeAssignRHSF (Ann (Call symbol exprs) (pos, _)) =
  lookUpSymbol symbol >>= \maybeT ->
  case maybeT of
    Nothing -> throwError $ "function symbol not found"
    Just (Ann t _) -> 
      case t of
        TFunc (Ann tOut _) tIns -> mapM analyzeExprF exprs >>= \exprs' ->
                           return $ Ann (Call symbol exprs') (pos, tOut)
        otherwise -> throwError $ "not a function symbol"

-- TODO: fix list of expressions' print out message
analyzeAssignRHSF (Ann (ArrayLiter []) (pos, _)) =
  return $ Ann (ArrayLiter []) (pos, TArray (Ann Any (pos, None)))
  
analyzeAssignRHSF (Ann (ArrayLiter exprs) (pos, _)) = 
  mapM analyzeExprF exprs >>= \exprs' ->
  if nub (map getT exprs') /= []
  then throwError "does not all have the same type"
  else return $ Ann (ArrayLiter exprs') (pos, getT $ head $ exprs')


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
  if t1 /= t2
  then throwError $ "binop expr not matching"
  else return $ Ann (BExpr bop e1' e2') (pos, t1)

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


