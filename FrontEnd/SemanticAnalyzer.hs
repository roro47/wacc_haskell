module FrontEnd.SemanticAnalyzer where

import qualified Data.List as List
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
type SymbolTs = [SymbolT]
type AnalysisState = ([SymbolT], Context)
type Analyzer = StateT AnalysisState (Either String)

builtInPos = newPos "builtIn" 0 0
getStats (Ann (StatList stats) _) = stats

getT (Ann _ (pos, t)) = t

unwrap (Ann x _) = x

match :: String -> (Ann f) -> [Type] -> (Analyzer ())
match msg a@(Ann x (pos, t)) expectT =
  if not $ elem t expectT
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

pushScope :: Analyzer ()
pushScope = do { (tables, context) <- get; put (HashMap.empty:tables, context) }

-- pre : stack of scope must be not empty
popScope :: Analyzer ()
popScope = do
           (tables, context) <- get
           put $ (tail tables, context)

addSymbol :: IdentF () -> Type -> SourcePos -> Analyzer ()
addSymbol id t pos = do (table:tables, context) <- get
                        put ((insert symbol (t, pos) table):tables, context)
                    where Ann (Ident symbol) _ = id

lookUpSymbol :: IdentF () -> Analyzer (Maybe (Type, SourcePos))
lookUpSymbol id =
  do
    (tables, _) <- get
    return $ foldl (<|>) Nothing $ map (HashMap.lookup symbol) tables
      where Ann (Ident symbol) _ = id

lookUpSymbolCurrScope :: IdentF () -> Analyzer (Maybe (Type, SourcePos))
lookUpSymbolCurrScope id =
  do
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
analyzeProgramF p@(Ann (Program fs stat) ann@(pos, none)) =
  do
    pushScope
    mapM (\(id, funcT) -> addSymbol (Ann (Ident id) (pos, None)) funcT builtInPos) builtInFunc
    fs' <- mapM analyzeFuncF fs
    stat' <- analyzeStatListF stat
    popScope
    return $ Ann (Program fs' stat') ann

analyzeFuncF :: FuncF () -> Analyzer (FuncF ())
analyzeFuncF f@(Ann (Func t symbol ps stats) (pos, none)) =
    lookUpSymbol symbol >>= \maybeT ->
      case maybeT of
        Just (_, pos') -> throwError $ declaredError symbol pos pos'
        otherwise ->
          setContext (FContext (TFunc [] paramTs t)) >>
          pushScope >>
          mapM (\(Ann (Param t pName) (pos, _)) -> addSymbol pName t pos) ps >>
          analyzeStatListF stats >>= \stats' ->
          popScope >>
          setContext Main >>
          addSymbol symbol (TFunc [] paramTs t) pos >>
          return f
  where paramTs = map (\(Ann (Param t _) _) -> t) ps

analyzeStatListF :: StatListF () -> Analyzer (StatListF ())
analyzeStatListF (Ann (StatList stats) ann) =
  mapM analyzeStatF stats >>= \stats' ->
  return $ Ann (StatList stats') ann

typeCheckArray :: String -> Type -> ExprF () -> Analyzer (ExprF ())
typeCheckArray msg (TArray t) rhs@(Ann (ArrayLiter exprs) _) =
  mapM (\e -> match msg e [t]) exprs >>
  return rhs

typeCheckArray msg t rhs =
  match msg rhs [t] >> return rhs

analyzeFuncAppF (Ann (FuncApp symbol exprs) (pos, _)) =
  lookUpSymbol symbol >>= \maybeT ->
  case maybeT of
    Nothing -> throwError $ notDeclaredError symbol pos
    Just (funcT@(TFunc _ paramTs _), _) ->
      mapM analyzeExprF exprs >>= \exprs' ->
      if length paramTs /= length exprs'
      then throwError ("For function call " ++ show symbol ++ " at " ++ show pos
                        ++", require " ++ show (length paramTs) ++ " parameters, " ++
                        show (length exprs') ++ " are given.\n")
      else foldM evalT funcT exprs' >>= \(TFunc _ _ returnT) ->
           return $ Ann (FuncApp symbol exprs') (pos, returnT)
    otherwise -> throwError $ "Symbol " ++ show symbol ++ " at " ++ show pos ++
                              " is not a function symbol"
  where evalT :: Type -> ExprF () -> Analyzer Type
        evalT (TFunc [] (paramT:paramTs) returnT) expr
          = do { match "" expr [paramT]; return $ TFunc [] paramTs returnT }
        evalT (TFunc allowedT (paramT:paramTs) returnT) expr@(Ann _ (_, t))
          = match "" expr allowedT >>
            if returnT == T
            then return $ TFunc [] (replace T t paramTs) t
            else return $ TFunc [] (replace T t paramTs) returnT
        -- Pre :: t could be only of one type
        replace :: Eq a => a -> a -> [a] -> [a]
        replace from to list = [ if x == from then to else x | x <- list]

analyzeStatF :: StatF () -> Analyzer (StatF ())
analyzeStatF (Ann s@(Declare declareT symbol rhs) (pos, none)) =
  lookUpSymbolCurrScope symbol >>= \maybeT ->
    case maybeT of
      Just (_, pos') -> throwError $ (declaredError symbol pos pos')
      otherwise -> analyzeExprF rhs >>= \rhs'@(Ann _ (pos, rhsT)) ->
                   if declareT == arrayT
                   then typeCheckArray err declareT rhs' >>
                        addSymbol symbol declareT pos >>
                        (return $ Ann (Declare declareT symbol rhs') (pos, none))
                   else  if rhsT /= declareT
                         then throwError $ typeError err pos [declareT] rhsT
                         else addSymbol symbol declareT pos >>
                              (return $ Ann (Declare declareT symbol rhs') (pos, none))
   where err = "Assign wrong type of value in declaration"
         getExprs (ArrayLiter exprs) = exprs

analyzeStatF (Ann (Assign lhs rhs) ann@(pos, none)) =
    analyzeExprF lhs >>= \ lhs'@(Ann _ (pos1, lT)) ->
    analyzeExprF rhs >>= \ rhs'@(Ann expr (pos2, rT)) ->
    case lT of
      TArray t -> case rT of
                  TArray _ -> mapM_ (\e -> match errArray e [t]) (getExprs expr) >>
                                (return $ Ann (Assign lhs' (Ann expr (pos2, lT))) ann)
                  otherwise -> throwError $ typeError err pos2 [lT] rT
      otherwise -> if lT == rT
                   then return $ Ann (Assign lhs' rhs') (pos, none)
                   else throwError $ typeError err pos2 [lT] rT
  where err = "Assign wrong type of value"
        errArray = "Wrong type of value in array"
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
analyzeExprF (Ann e@(IdentExpr symbol) (pos, _)) =
  lookUpSymbol symbol >>= \maybeT ->
    case maybeT of
      Nothing -> throwError $ notDeclaredError symbol pos
      Just (t, _) -> return $ Ann e (pos, t)

analyzeExprF (Ann (ArrayLiter exprs) (pos, _)) =
  mapM analyzeExprF exprs >>= \exprs' ->
  return $ Ann (ArrayLiter exprs') (pos, TArray TAny)

analyzeExprF (Ann e@(ArrayElem symbol exprs) (pos, _)) =
  lookUpSymbol symbol >>= \maybeT ->
    case maybeT of
      Nothing -> throwError $ notDeclaredError symbol pos
      Just (TArray t, _) ->
        (mapM analyzeExprF exprs >>= \exprs' ->
         case List.findIndex (t /=) (map getT exprs') of
            Just i -> throwError "some expr has incorrect type"
            Nothing -> return $ Ann (ArrayElem symbol exprs') (pos, t)
        )
      otherwise -> throwError "type is not array"

analyzeExprF (Ann (BracketExpr expr) (pos, _)) =
  analyzeExprF expr >>= \expr' ->
  return $ Ann (BracketExpr expr') (pos, getT expr')

analyzeExprF (Ann (FuncExpr f) (pos, _)) =
  analyzeFuncAppF f >>= \f'@(Ann _ (_, t)) ->
  return $ Ann (FuncExpr f') (pos, t)

analyzeFile :: String -> IO (ProgramF ())
analyzeFile file =
  do
    program <- readFile file
    case parse parseProgramF "" program of
       Left e -> print e >>
                fail ("parse error: at " ++ show (errorPos e) ++ " with file " ++ file)
       Right p -> case evalStateT (analyzeProgramF p) ([], Main) of
                    Left e -> putStr e >> fail ""
                    Right p' -> return p'
