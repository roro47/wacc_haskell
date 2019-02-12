{-# LANGUAGE FlexibleInstances #-}

module FrontEnd.SemanticAnalyzer where

import qualified Data.List as List
import qualified Data.List.Utils as ListUtils
import Control.Applicative
import Control.Monad.State
import Control.Monad.Except
import Data.HashMap as HashMap hiding (map)
import Text.ParserCombinators.Parsec hiding ((<|>))
import Text.ParserCombinators.Parsec.Pos
import FrontEnd.AST
import FrontEnd.Parser


data Context = Main | FContext Type deriving (Show, Eq)

type SymbolT = Map String (Type, SourcePos)  -- declaration is stored at type
type AnalysisState = ([SymbolT], Context)
type Analyzer = StateT AnalysisState (Either String)

builtInPos = newPos "builtIn" 0 0
getStats (Ann (StatList stats) _) = stats

getT (Ann _ (pos, t)) = T

class Matchable a where
  match :: String -> (Ann a) -> [Type] -> (Analyzer ())

instance Matchable (Expr ()) where
 match msg a@(Ann (ArrayLiter exprs) (pos, t)) [TArray expectT] =
   mapM_ (\e -> match msg e [expectT]) exprs

 match msg a@(Ann x (pos, t)) expectT =
   matchT msg t expectT pos

instance Matchable (Stat ()) where
 match msg a@(Ann x (pos, t)) expectT =
   matchT msg t expectT pos

matchT :: String -> Type -> [Type] -> SourcePos -> (Analyzer ())
matchT msg t expectT pos
  = if not $ elem t expectT
    then throwError $ typeError msg pos expectT t
    else return ()

-- errors
declaredError :: IdentF () -> SourcePos -> SourcePos -> String
declaredError symbol pos1 pos2
 = "symbol " ++ show symbol ++ " at " ++ show pos1 ++ " already declared at "  ++ show pos2

notDeclaredError :: IdentF () -> SourcePos -> String
notDeclaredError symbol pos
 = "symbol " ++ show symbol ++ " at " ++ show pos ++ " not defined."

typeError :: String -> SourcePos -> [Type] -> Type -> String
typeError msg pos expected actual =
  "type not matched at " ++ show pos ++ "\n" ++
  msg' ++
  "expected : " ++ (concat $ List.intersperse "," (map show expected)) ++ "\n" ++
  "actual: " ++ (show actual) ++ "\n"
  where msg' = if msg /= "" then msg ++ "\n" else msg

paramLenError :: IdentF () -> SourcePos -> Int -> Int -> String
paramLenError symbol pos paramLen exprLen =
  "For function call " ++ show symbol ++ " at " ++ show pos
   ++", require " ++ show paramLen ++ " parameters, " ++
   show exprLen ++ " are given.\n"

pushScope :: Analyzer ()
pushScope = do { (tables, context) <- get; put (HashMap.empty:tables, context) }

-- pre : stack of scope must be not empty
popScope :: Analyzer ()
popScope = do { (tables, context) <- get; put (tail tables, context) }

addSymbol :: IdentF () -> Type -> SourcePos -> Analyzer ()
addSymbol id t pos = do
  (table:tables, context) <- get
  put ((insert symbol (t, pos) table):tables, context)
  where Ann (Ident symbol) _ = id

lookUpSymbol :: IdentF () -> Analyzer (Maybe (Type, SourcePos))
lookUpSymbol id = do
  (tables, _) <- get
  return $ foldl (<|>) Nothing $ map (HashMap.lookup symbol) tables
    where Ann (Ident symbol) _ = id

lookUpSymbolCurrScope :: IdentF () -> Analyzer (Maybe (Type, SourcePos))
lookUpSymbolCurrScope id = do
  (tables, _) <- get
  case tables of
    [] -> return Nothing
    (t:ts) -> return $ HashMap.lookup symbol t
  where Ann (Ident symbol) _ = id

setContext :: Context -> Analyzer ()
setContext c = do { (tables, _) <- get; put (tables, c) }

getContext :: Analyzer Context
getContext = do { (_, c) <- get; return c }

analyzeProgramF :: ProgramF () -> Analyzer (ProgramF ())
analyzeProgramF p@(Ann (Program fs stat) ann@(pos, none)) = do
  pushScope
  mapM (\(id, funcT) -> addSymbol (Ann (Ident id) (pos, None)) funcT builtInPos) builtInFunc
  fs' <- mapM analyzeFuncF fs
  stat' <- analyzeStatListF stat
  popScope
  return $ Ann (Program fs' stat') ann

analyzeFuncF :: FuncF () -> Analyzer (FuncF ())
analyzeFuncF f@(Ann (Func t symbol ps stats) (pos, none)) = do
  maybeT <- lookUpSymbol symbol
  case maybeT of
     Just (_, pos') -> throwError $ declaredError symbol pos pos'
     otherwise -> do
       setContext (FContext (TFunc [] paramTs t))
       pushScope
       mapM (\(Ann (Param t pName) (pos, _)) -> addSymbol pName t pos) ps
       stats' <- analyzeStatListF stats
       popScope
       setContext Main
       addSymbol symbol (TFunc [] paramTs t) pos
       return f
  where paramTs = map (\(Ann (Param t _) _) -> t) ps

analyzeStatListF :: StatListF () -> Analyzer (StatListF ())
analyzeStatListF (Ann (StatList stats) ann) =
  mapM analyzeStatF stats >>= \stats' ->
  return $ Ann (StatList stats') ann

analyzeFuncAppF (Ann (FuncApp symbol exprs) (pos, _)) = do
    maybeT <- lookUpSymbol symbol
    case maybeT of
      Nothing -> throwError $ notDeclaredError symbol pos
      Just (funcT@(TFunc _ paramTs _), _) ->
        do
          exprs' <- mapM analyzeExprF exprs
          checkParamLen symbol pos (length paramTs) (length exprs')
          (TFunc _ _ returnT) <- foldM evalT funcT exprs'
          return $ Ann (FuncApp symbol exprs') (pos, returnT)
      otherwise -> throwError $ "Symbol " ++ show symbol ++ " at " ++ show pos ++
                              " is not a function symbol"

  where evalT :: Type -> ExprF () -> Analyzer Type
        evalT (TFunc [] (paramT:paramTs) returnT) expr
          = do { match "" expr [paramT]; return $ TFunc [] paramTs returnT }

        evalT (TFunc allowedT (paramT:paramTs) returnT) expr@(Ann _ (_, t))
          = match "" expr allowedT >>= \_ ->
            if returnT == T
            then return $ TFunc [] (ListUtils.replace [T] [t] paramTs) t
            else return $ TFunc [] (ListUtils.replace [T] [t] paramTs) returnT

        checkParamLen :: IdentF () -> SourcePos -> Int -> Int -> Analyzer ()
        checkParamLen symbol pos paramLen exprLen
         = if paramLen /= exprLen then throwError (paramLenError symbol pos paramLen exprLen)
           else return ()

analyzeStatF :: StatF () -> Analyzer (StatF ())
analyzeStatF (Ann s@(Declare declareT symbol rhs) (pos, none)) =
  lookUpSymbolCurrScope symbol >>= \maybeT ->
    case maybeT of
      Just (_, pos') -> throwError $ declaredError symbol pos pos'
      otherwise -> do rhs' <- analyzeExprF rhs
                      match err rhs' [declareT]
                      addSymbol symbol declareT pos
                      return $ Ann (Declare declareT symbol rhs') (pos, none)
   where err = "Assign wrong type of value in declaration"

analyzeStatF (Ann (Assign lhs rhs) ann@(pos, none)) = do
  lhs'@(Ann _ (pos1, lT)) <- analyzeExprF lhs
  rhs'@(Ann expr (pos2, rT)) <- analyzeExprF rhs
  match err rhs' [lT]
  return $ Ann (Assign lhs' rhs') ann
  where err = "Assign wrong type of value"
        getExprs (ArrayLiter exprs) = exprs

analyzeStatF (Ann (Return expr) ann@(pos, none)) =
  getContext >>= \c ->
  case c of
    Main -> throwError $ "Attempt to return from main scope at " ++ show pos
    FContext (TFunc _ _ returnT) ->
      analyzeExprF expr >>= \expr' ->
      match "Function return type not matched" expr' [returnT] >>= \_ ->
      return $ Ann (Return expr') (pos, getT expr')
    -- in other case, should throw non-exhaustive pattern match failure

analyzeStatF (Ann (Exit expr) ann) =
  do expr' <- analyzeExprF expr
     match err expr' [TInt]
     return $ Ann (Exit expr') ann
  where err = "Exit value not of type int"

analyzeStatF (Ann (FuncStat f) ann) =
  do { f' <- analyzeFuncAppF f ; return $ Ann (FuncStat f') ann }

analyzeStatF (Ann (If expr stat1 stat2) ann) =
  do expr' <- analyzeExprF expr
     match err expr' [TBool]
     stat1' <- analyzeStatListF stat1
     stat2' <- analyzeStatListF stat2
     return $ Ann (If expr' stat1' stat2') ann
  where err = "Condition must be of type bool"

analyzeStatF (Ann (While expr stat) ann) =
  do expr' <- analyzeExprF expr
     match err expr' [TBool]
     stat' <- analyzeStatListF stat
     return $ Ann (While expr' stat') ann
  where err = "Condition must be of type bool"

analyzeStatF (Ann (Subroutine stat) ann) =
  do pushScope
     stat' <- analyzeStatListF stat
     popScope
     return $ Ann (Subroutine stat') ann

analyzeExprF :: ExprF () -> Analyzer (ExprF ())
analyzeExprF (Ann liter@(IntLiter _) (pos, _)) = return $ Ann liter (pos, TInt)
analyzeExprF (Ann liter@(BoolLiter _) (pos,_)) = return $ Ann liter (pos, TBool)
analyzeExprF (Ann liter@(CharLiter _) (pos, _)) = return $ Ann liter (pos, TChar)
analyzeExprF (Ann liter@(StringLiter _) (pos, _)) = return $ Ann liter (pos, TStr)
analyzeExprF a@(Ann Null (pos, _)) = return a
analyzeExprF (Ann e@(IdentExpr symbol) (pos, _)) = do
  maybeT <- lookUpSymbol symbol
  case maybeT of
    Nothing -> throwError $ notDeclaredError symbol pos
    Just (t, _) -> return $ Ann e (pos, t)

analyzeExprF (Ann (ArrayLiter exprs) (pos, _)) = do
  exprs' <- mapM analyzeExprF exprs
  return $ Ann (ArrayLiter exprs') (pos, TArray TAny)

analyzeExprF (Ann e@(ArrayElem symbol exprs) (pos, _)) = do
  maybeT <- lookUpSymbol symbol
  case maybeT of
    Nothing -> throwError $ notDeclaredError symbol pos
    Just (t, _) -> do
      matchT "" t [TArray TAny] pos
      exprs' <- mapM analyzeExprF exprs
      mapM (\e -> match "Array Index has incorrect type" e [TInt]) exprs'
      let { TArray elemT = t }
      return $ Ann (ArrayElem symbol exprs') (pos, elemT)

analyzeExprF (Ann (BracketExpr expr) (pos, _)) = do
  expr' <- analyzeExprF expr
  return $ Ann (BracketExpr expr') (pos, getT expr')

analyzeExprF (Ann (FuncExpr f) (pos, _)) = do
  f'@(Ann _ (_, t)) <- analyzeFuncAppF f
  return $ Ann (FuncExpr f') (pos, t)

analyzeAST :: ProgramF () -> IO (ProgramF ())
analyzeAST ast = do
  case evalStateT (analyzeProgramF ast) ([], Main) of
    Left e -> putStr e >> fail ""
    Right p' -> return p'
