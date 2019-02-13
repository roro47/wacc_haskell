{-# LANGUAGE FlexibleInstances #-}

module FrontEnd.SemanticAnalyzer where

import System.Exit
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
type AnalysisState = (([SymbolT], SymbolT), Context) -- ((scopeTable, functionTable), current context)
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
 = "Symbol " ++ show symbol ++ " at " ++ show pos1 ++ " already declared at "  ++ show pos2 ++ "\n"

notDeclaredError :: IdentF () -> SourcePos -> String
notDeclaredError symbol pos
 = "Symbol " ++ show symbol ++ " at " ++ show pos ++ " not defined.\n"

typeError :: String -> SourcePos -> [Type] -> Type -> String
typeError msg pos expected actual =
  "Type not matched at " ++ show pos ++ "\n" ++
  msg' ++
  "expected : " ++ (concat $ List.intersperse "," (map show expected)) ++ "\n" ++
  "actual: " ++ (show actual) ++ "\n"
  where msg' = if msg /= "" then msg ++ "\n" else msg

paramLenError :: IdentF () -> Int -> Int -> String
paramLenError symbol paramLen exprLen =
  "For " ++ functionType symbol ++ ", require " ++ 
  show paramLen ++ " parameters, " ++
  show exprLen ++ " are given.\n"

-- whether function is builtIn or user-defined
functionType :: IdentF () -> String
functionType (Ann (Ident s) (pos, _))
  | elem s (map fst builtInFunc) = "built-in function " ++ "\"" ++ s ++ "\""
  | otherwise = "function " ++ "\"" ++ s ++ "\"" ++ " defined at " ++ show pos

pushScope :: Analyzer ()
pushScope = do 
  ((tables, fTable), context) <- get 
  put ((HashMap.empty:tables, fTable), context)

-- pre : stack of scope must be not empty
popScope :: Analyzer ()
popScope = do 
  ((tables, fTable), context) <- get
  put ((tail tables, fTable), context)

addSymbol :: IdentF () -> Type -> SourcePos -> Analyzer ()
addSymbol id t pos = do
  ((tables, fTable), context) <- get
  guard $ tables /= []
  put (((insert symbol (t, pos) (head tables)):(tail tables), fTable), context)
  where Ann (Ident symbol) _ = id

addFunctionSymbol :: IdentF () -> Type -> SourcePos -> Analyzer ()
addFunctionSymbol id t pos = do
  ((sTables, fTable), context) <- get
  put ((sTables, (insert symbol (t, pos) fTable)), context)
  where Ann (Ident symbol) _ = id

lookUpSymbol :: IdentF () -> Analyzer (Maybe (Type, SourcePos))
lookUpSymbol id = do
  ((tables, fTable),  _) <- get
  return $ foldl (<|>) Nothing $ map (HashMap.lookup symbol) tables
    where Ann (Ident symbol) _ = id

lookUpSymbolCurrScope :: IdentF () -> Analyzer (Maybe (Type, SourcePos))
lookUpSymbolCurrScope id = do
  ((tables,_),  _) <- get
  case tables of
    [] -> return Nothing
    (t:ts) -> return $ HashMap.lookup symbol t
  where Ann (Ident symbol) _ = id

lookUpFunction :: IdentF () -> Analyzer (Maybe (Type, SourcePos))
lookUpFunction id = do
  ((_, fTable),  _) <- get
  return $ HashMap.lookup symbol fTable
  where Ann (Ident symbol) _ = id
    
setContext :: Context -> Analyzer ()
setContext c = do { (tables, _) <- get; put (tables, c) }

getContext :: Analyzer Context
getContext = do { (_, c) <- get; return c }

analyzeProgramF :: ProgramF () -> Analyzer (ProgramF ())
analyzeProgramF p@(Ann (Program fs stat) ann@(pos, none)) = do
  pushScope
  mapM (\(id, funcT) -> addFunctionSymbol (Ann (Ident id) (pos, None)) funcT builtInPos) builtInFunc
  mapM getAllFuncType fs
  fs' <- mapM analyzeFuncF fs
  stat' <- analyzeStatListF stat
  popScope
  return $ Ann (Program fs' stat') ann

getAllFuncType :: FuncF () -> Analyzer (FuncF ())
getAllFuncType f@(Ann (Func t symbol ps stats) (pos, none)) = do
  maybeT <- lookUpFunction symbol
  case maybeT of
     Just (_, pos') -> throwError $ declaredError symbol pos pos'
     otherwise -> do
       addFunctionSymbol symbol (TFunc [] paramTs t) pos
       return f
  where paramTs = map (\(Ann (Param t _) _) -> t) ps

analyzeFuncF :: FuncF () -> Analyzer (FuncF ())
analyzeFuncF f@(Ann (Func t symbol ps stats) (pos, none)) = do
  setContext (FContext (TFunc [] paramTs t))
  pushScope
  mapM (\(Ann (Param t pName) (pos, _)) -> addSymbol pName t pos) ps
  stats' <- analyzeStatListF stats
  popScope
  setContext Main
  return f
  where paramTs = map (\(Ann (Param t _) _) -> t) ps

analyzeStatListF :: StatListF () -> Analyzer (StatListF ())
analyzeStatListF (Ann (StatList stats) ann) =
  mapM analyzeStatF stats >>= \stats' ->
  return $ Ann (StatList stats') ann

analyzeFuncAppF (Ann (FuncApp symbol exprs) (pos, _)) = do
    maybeT <- lookUpFunction symbol
    case maybeT of
      Nothing -> throwError $ notDeclaredError symbol pos
      Just (funcT@(TFunc _ paramTs _), _) ->
        do
          exprs' <- mapM analyzeExprF exprs
          checkParamLen symbol pos (length paramTs) (length exprs')
          funcT' <- foldM evalT funcT exprs'
          guard (isTFunc funcT')
          let { TFunc _ _ returnT = funcT' }
          return $ Ann (FuncApp symbol exprs') (pos, returnT)
      otherwise -> throwError $ "Symbol " ++ show symbol ++ " at " ++ show pos ++
                              " is not a function symbol"

  where errT = "For call of " ++ functionType symbol ++ ", type is not matched."
        evalT :: Type -> ExprF () -> Analyzer Type
        evalT (TFunc [] (paramT:paramTs) returnT) expr
          = do { match errT expr [paramT]; return $ TFunc [] paramTs returnT }

        evalT (TFunc allowedT (paramT:paramTs) returnT) expr@(Ann _ (_, t))
          = match errT expr allowedT >>= \_ ->
            decideT t paramT >>= \passT ->
            return $ TFunc [] (map (replace passT) paramTs) (replace passT returnT)

        checkParamLen :: IdentF () -> SourcePos -> Int -> Int -> Analyzer ()
        checkParamLen symbol pos paramLen exprLen
         = if paramLen /= exprLen then throwError (paramLenError symbol paramLen exprLen)
           else return ()

        -- decide which type replace T
        decideT :: Type -> Type -> Analyzer Type
        decideT (TPair t1 t2) (TPair T _) = return t1
        decideT (TPair t1 t2) (TPair _ T) = return t2
        decideT (TArray t)    (TArray T) = return t
        decideT t T = return t
        decide _ = None
        
        replace :: Type -> Type -> Type
        replace passT (TPair t1 t2) = TPair (replace passT t1) (replace passT t2)
        replace passT (TArray t) = TArray (replace passT t)
        replace passT t@(TFunc _ _ _) = t -- not decided behaviour
        replace passT T = passT
        replace passT t = t

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
    Main -> throwError $ "Attempt to return from main scope at " ++ show pos ++ "\n"
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
     pushScope
     stat1' <- analyzeStatListF stat1
     popScope
     pushScope
     stat2' <- analyzeStatListF stat2
     popScope
     return $ Ann (If expr' stat1' stat2') ann
  where err = "Condition must be of type bool"

analyzeStatF (Ann (While expr stat) ann) =
  do expr' <- analyzeExprF expr
     match err expr' [TBool]
     pushScope
     stat' <- analyzeStatListF stat
     popScope
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
analyzeExprF a@(Ann Null (pos, _)) = return $ Ann Null (pos, TPair TAny TAny)
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
      matchT "" t [TArray TAny, TStr] pos
      exprs' <- mapM analyzeExprF exprs
      mapM (\e -> match "Array Index has incorrect type" e [TInt]) exprs'
      case t of
        TStr -> return $ Ann (ArrayElem symbol exprs') (pos, TChar)
        TArray elemT ->
          return $ Ann (ArrayElem symbol exprs') (pos, elemT)
        otherwise -> throwError "unexpected error at analyzeArrayElem"

analyzeExprF (Ann (BracketExpr expr) (pos, _)) = do
  expr'@(Ann _ (pos, t)) <- analyzeExprF expr
  return $ Ann (BracketExpr expr') (pos, t)

analyzeExprF (Ann (FuncExpr f) (pos, _)) = do
  f'@(Ann _ (_, t)) <- analyzeFuncAppF f
  return $ Ann (FuncExpr f') (pos, t)

analyzeAST :: ProgramF () -> IO (ProgramF ())
analyzeAST ast = do
  case evalStateT (analyzeProgramF ast) (([], HashMap.empty), Main) of
    Left e -> fail "#semantic_error#" >>
              exitWith (ExitFailure 200)
    Right p' -> return p'
