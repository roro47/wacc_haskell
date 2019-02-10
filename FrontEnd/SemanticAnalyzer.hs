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


data Context = Main | FContext (Type ()) deriving (Show, Eq)

type SymbolT = Map String (TypeF ())  -- declaration is stored at type
type SymbolTs = [SymbolT]
type AnalysisState = ([SymbolT], Context) 
type Analyzer = StateT AnalysisState (Either String)


getStats (Ann (StatList stats) _) = stats

getT (Ann _ (pos, t)) = t

unwrap (Ann x _) = x

match :: String -> (Ann f) -> [Type ()] -> (Analyzer ())
match msg a@(Ann x (pos, t)) expectT =
  if not $ elem t expectT
  then throwError $ typeError msg pos expectT t
  else return ()

-- dummy type
dummyAnn = (newPos "d" 1 2, None)
anyType = (Ann Any dummyAnn)
pairType = TPair anyType anyType
arrayType = TArray anyType

-- errors
declaredError :: IdentF () -> SourcePos -> SourcePos -> String
declaredError symbol pos1 pos2
 = "symbol " ++ show symbol ++ " at " ++ show pos1 ++ " already declared at "  ++ show pos2

notDeclaredError :: IdentF () -> SourcePos -> String
notDeclaredError symbol pos
 = "symbol " ++ show symbol ++ " at " ++ show pos ++ " not defined."

typeError :: String -> SourcePos -> [Type ()] -> Type () -> String
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

addSymbol :: IdentF () -> TypeF () -> Analyzer ()
addSymbol id t = do
                 (table:tables, context) <- get
                 put ((insert symbol t table):tables, context)
                    where Ann (Ident symbol) _ = id

lookUpSymbol :: IdentF () -> Analyzer (Maybe (TypeF ()))
lookUpSymbol id =
  do
    (tables, _) <- get
    return $ foldl (<|>) Nothing $ map (HashMap.lookup symbol) tables
      where Ann (Ident symbol) _ = id

lookUpSymbolCurrScope :: IdentF () -> Analyzer (Maybe (TypeF ()))
lookUpSymbolCurrScope id = 
  do
    (tables, _) <- get
    case tables of
      [] -> return Nothing
      (t:ts) -> return $ HashMap.lookup symbol t
  where Ann (Ident symbol) _ = id


setContext :: Context -> Analyzer ()
setContext c = do { (tables, _) <- get; put (tables, c) }

getContext :: Analyzer (Context)
getContext = do { (_, c) <- get; return c }

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
        Just (Ann _ (pos', _)) -> throwError $ declaredError symbol pos pos'
        otherwise ->
          setContext (FContext (TFunc t paramTs)) >>
          pushScope >>
          mapM (\(Ann (Param t pName) _) -> addSymbol pName t) ps >>
          analyzeStatListF stats >>= \stats' ->
          popScope >>
          setContext Main >> 
          addSymbol symbol (Ann (TFunc t paramTs) (pos, none)) >>
          return f
  where paramTs = map (\(Ann (Param t _) _) -> t) ps

analyzeStatListF :: StatListF () -> Analyzer (StatListF ())
analyzeStatListF (Ann (StatList stats) ann) =
  mapM analyzeStatF stats >>= \stats' ->
  return $ Ann (StatList stats') ann

typeCheckExpr :: String -> Type () -> ExprF () -> Analyzer (ExprF ())
typeCheckExpr err t e@(Ann expr (posExpr, exprT))
   = if t /= exprT
     then throwError $ typeError err posExpr [t] exprT
     else return e


typeCheckArray :: String -> TypeF () -> AssignRHSF () -> Analyzer (AssignRHSF ())
typeCheckArray msg (Ann t _) rhs@(Ann (ExprRHS expr) _) =
  typeCheckExpr msg t expr >>
  return rhs

typeCheckArray msg (Ann t _) rhs@(Ann (ArrayLiter exprs) _) =
  mapM (typeCheckExpr msg arrayT) exprs >>
  return rhs
  where (TArray (Ann arrayT _)) = t

analyzeStatF :: StatF () -> Analyzer (StatF ())

analyzeStatF s@(Ann Skip _) = return $ s

analyzeStatF (Ann s@(Declare t symbol rhs) (pos, none)) =
  lookUpSymbolCurrScope symbol >>= \maybeT ->
    case maybeT of
      Just (Ann _ (pos', _)) -> throwError $ (declaredError symbol pos pos') 
      otherwise -> analyzeAssignRHSF rhs >>= \rhs'@(Ann _ (pos, rhsT)) ->
                   if declareT == arrayType
                   then typeCheckArray err t rhs' >>
                        addSymbol symbol t >>
                        (return $ Ann (Declare t symbol rhs') (pos, none))
                   else  if rhsT /= declareT
                         then throwError $ typeError err pos [declareT] rhsT
                         else addSymbol symbol t >>
                              (return $ Ann (Declare t symbol rhs') (pos, none))
   where (Ann declareT _ ) = t
         err = "Assign wrong type of value in declaration"
         getExprs (ArrayLiter exprs) = exprs

analyzeStatF (Ann (Assign lhs rhs) ann@(pos, none)) =
    analyzeAssignLHSF lhs >>= \ lhs'@(Ann _ (pos1, lT)) ->
    analyzeAssignRHSF rhs >>= \ rhs'@(Ann expr (pos2, rT)) ->
    case lT of
      TArray (Ann t _) -> case rT of
                    TArray _ -> mapM_ (typeCheckExpr errArray t) (getExprs expr) >>
                                (return $ Ann (Assign lhs' (Ann expr (pos2, lT))) ann)
                    otherwise -> throwError $ typeError err pos2 [lT] rT
      otherwise -> if lT == rT
                   then return $ Ann (Assign lhs' rhs') (pos, none)
                   else throwError $ typeError err pos2 [lT] rT
  where err = "Assign wrong type of value"
        errArray = "Wrong type of value in array"
        getExprs (ArrayLiter exprs) = exprs


analyzeStatF (Ann (Read lhs) (pos, _)) =
  analyzeAssignLHSF lhs >>= \lhs'@(Ann _ (_, tl)) ->
  match err lhs' [TInt, TChar] >>= \_ ->
  return $ Ann (Read lhs') (pos, tl)
  where err = "Support reading int and char only"

analyzeStatF (Ann (Free expr) ann) =
  analyzeExprF expr >>= \expr'@(Ann _ (posExpr, t)) ->
  match err expr' [arrayType, pairType] >>= \_ ->
  return $ Ann (Free expr') ann
  where err = "Support freeing array and pair only"

analyzeStatF (Ann (Return expr) ann@(pos, none)) =
  getContext >>= \c ->
  case c of
    Main -> throwError $ "Attempt to return from main scope at " ++ show pos
    FContext (TFunc (Ann returnT _) _) ->
      analyzeExprF expr >>= \expr' ->
      match "Function return type not matched" expr' [returnT] >>= \_ ->
      return $ Ann (Return expr') (pos, getT expr')
    -- in other case, should throw non-exhaustive pattern match failure
  
analyzeStatF (Ann (Exit expr) ann) =
  analyzeExprF expr >>= \expr' ->
  match err expr' [TInt] >>= \_ ->
  return $ Ann (Exit expr') ann
  where err = "Exit value not of type int"

analyzeStatF (Ann (Print expr) ann) =
  analyzeExprF expr >>= \expr' ->
  return $ Ann (Print expr') ann

analyzeStatF (Ann (Println expr) ann) =
  analyzeExprF expr >>= \expr' ->
  return $ Ann (Println expr') ann

analyzeStatF (Ann (If expr stat1 stat2) ann) =
  analyzeExprF expr >>= \expr' ->
  match err expr' [TBool] >>= \_ ->
  analyzeStatListF stat1 >>= \stat1' ->
  analyzeStatListF stat2 >>= \stat2' ->
  return $ Ann (If expr' stat1' stat2') ann
  where err = "Condition must be of type bool"

analyzeStatF (Ann (While expr stat) ann) =
  analyzeExprF expr >>= \expr' ->
  match err expr' [TBool] >>= \_ ->
  analyzeStatListF stat >>= \stat' ->
  return $ Ann (While expr' stat') ann
  where err = "Condition must be of type bool"

analyzeStatF (Ann (Subroutine stat) ann) =
  pushScope >>= \_ ->
  analyzeStatListF stat >>= \stat' ->
  popScope >>= \_ ->
  return $ Ann (Subroutine stat') ann

analyzeAssignLHSF :: AssignLHSF () -> Analyzer (AssignLHSF ())
analyzeAssignLHSF (Ann lhs@(IdentLHS symbol) (pos, _)) =
    lookUpSymbol symbol >>= \maybeT ->
      case maybeT of
        Just (Ann t _) -> return $ Ann lhs (pos, t)
        otherwise -> throwError $ notDeclaredError symbol pos

analyzeAssignLHSF (Ann (ArrayElemLHS a) (pos, _)) =
  analyzeArrayElemF a >>= \a' ->
  return $ Ann (ArrayElemLHS a') (pos, getT a')

analyzeAssignLHSF (Ann (PairElemLHS p) (pos, _)) =
  analyzePairElemF p >>= \p' ->
  return $ Ann (PairElemLHS p') (pos, getT p')

analyzePairElemF :: PairElemF () -> Analyzer (PairElemF ())
analyzePairElemF (Ann (PairElemFst expr) (pos, _)) =
  analyzeExprF expr >>= \expr'@(Ann _ (_, t)) ->
  case t of
    TPair t1 _ -> return $ Ann (PairElemFst expr') (pos, unwrap t1)
    otherwise -> throwError $ typeError "" pos [pairType] t

analyzePairElemF (Ann (PairElemSnd expr) (pos, _)) =
  analyzeExprF expr >>= \expr'@(Ann _ (_, t)) ->
  case t of
    TPair _ t2 -> return $ Ann (PairElemSnd expr') (pos, unwrap t2)
    otherwise -> throwError $ typeError "" pos [pairType] t

analyzeArrayElemF :: ArrayElemF () -> Analyzer (ArrayElemF ())
analyzeArrayElemF (Ann e@(ArrayElem symbol exprs) (pos, _)) =
  lookUpSymbol symbol >>= \maybeT ->
    case maybeT of
      Nothing -> throwError $ notDeclaredError symbol pos 
      Just (Ann (TArray (Ann t _)) _) ->
        (mapM analyzeExprF exprs >>= \exprs' ->
         case List.findIndex (t /=) (map getT exprs') of
            Just i -> throwError "some expr has incorrect type"
            Nothing -> return $ Ann (ArrayElem symbol exprs') (pos, t)
        )
      otherwise -> throwError "type is not array"

analyzeAssignRHSF :: AssignRHSF () -> Analyzer (AssignRHSF ())
analyzeAssignRHSF (Ann (ExprRHS expr) (pos, _)) =
  do
    expr' <- analyzeExprF expr
    return $ Ann (ExprRHS expr') (pos, getT expr')

analyzeAssignRHSF (Ann (NewPair expr1 expr2) (pos, _)) =
  do
    expr1'@(Ann _ (_, t1)) <- analyzeExprF expr1
    expr2'@(Ann _ (_, t2)) <- analyzeExprF expr2
    return $ Ann (NewPair expr1' expr2')
            (pos, TPair (Ann t1 (pos, None)) (Ann t2 (pos, None)))

analyzeAssignRHSF (Ann (PairElemRHS p) (pos, _)) =
  analyzePairElemF p >>= \p' ->
  return $ Ann (PairElemRHS p') (pos, getT p')


analyzeAssignRHSF (Ann (Call symbol exprs) (pos, _)) =
  lookUpSymbol symbol >>= \maybeT ->
  case maybeT of
    Nothing -> throwError $ notDeclaredError symbol pos
    Just (Ann (TFunc (Ann tOut _) tIns) _) ->
      mapM analyzeExprF exprs >>= \exprs' ->
      if length tIns /= length exprs'
      then throwError ("For function call " ++ show symbol ++ " at " ++ show pos
                        ++", require " ++ show (length tIns) ++ " parameters, " ++
                        show (length exprs') ++ " are given.\n")
      else mapM (\((Ann t _), e) -> match "" e [t]) (zip tIns exprs') >>= \_ ->
           return $ Ann (Call symbol exprs') (pos, tOut)
    otherwise -> throwError $ "Symbol " ++ show symbol ++ " at " ++ show pos ++
                              " is not a function symbol"

analyzeAssignRHSF (Ann (ArrayLiter []) (pos, _)) =
  return $ Ann (ArrayLiter []) (pos, TArray (Ann Any (pos, None)))

analyzeAssignRHSF (Ann (ArrayLiter exprs) (pos, _)) =
  mapM analyzeExprF exprs >>= \exprs' ->
  return $ Ann (ArrayLiter exprs') (pos, TArray (Ann Any (pos, None)))

uopTypeTable :: [(UnaryOp, ([Type ()], Type ()))]
uopTypeTable = [(Not, ([TBool], TBool)),
                (Len, ([arrayType], TInt)),
                (Ord, ([TChar], TInt)),
                (Neg, ([TInt], TInt)),
                (Pos, ([TInt], TInt)),
                (Chr, ([TInt], TChar))]

bopTypeTable  :: [(BinaryOp, ([Type ()], Type ()))]
bopTypeTable  =  [(Mul, ([TInt], TInt)),
                (Div, ([TInt], TInt)),
                (Mod, ([TInt], TInt)),
                (Plus, ([TInt], TInt)),
                (G, ([TInt, TChar], TBool)),
                (GEq, ([TInt, TChar], TBool)),
                (L, ([TInt, TChar], TBool)),
                (LEq, ([TInt, TChar], TBool)),
                (Eq, ([TInt, TChar, TBool, pairType, arrayType], TBool)),
                (NEq, ([TInt, TChar, TBool, pairType, arrayType], TBool)),
                (And, ([TBool], TBool)),
                (Or, ([TBool], TBool))]
                 
analyzeExprF :: ExprF () -> Analyzer (ExprF ())
analyzeExprF (Ann (LiterExpr i) (pos, _)) =
  do
    liter <- analyzeLiterF i
    return $ Ann (LiterExpr liter) (pos, getT liter)

analyzeExprF (Ann e@(IdentExpr symbol) (pos, _)) =
  lookUpSymbol symbol >>= \maybeT ->
    case maybeT of
      Nothing -> throwError $ notDeclaredError symbol pos
      Just (Ann t _) -> return $ Ann e (pos, t)

analyzeExprF (Ann (ArrayExpr a) (pos, _)) =
  analyzeArrayElemF a >>= \a' ->
  return $ Ann (ArrayExpr a') (pos, getT a')

analyzeExprF (Ann (UExpr uop e) (pos, _)) =
  analyzeExprF e >>= \e'->
  case List.lookup uop uopTypeTable of
    Just (inT, outT) -> match err e' inT >>= \_ ->
                        return $ Ann (UExpr uop e') (pos, outT)
    otherwise -> throwError "Unary operator not found"
  where err = "Type not matched between unary operator"

analyzeExprF (Ann (BExpr bop e1 e2) (pos, _)) =
  analyzeExprF e1 >>= \e1'@(Ann _ (pos, t1)) ->
  analyzeExprF e2 >>= \e2'@(Ann _ (_, t2)) ->
  case List.lookup bop bopTypeTable  of
   Just (inT, outT) -> match err e1' inT >>= \_ ->
                       match err e2' [t1] >>= \_ ->
                       return $ Ann (BExpr bop e1' e2') (pos, outT)
   otherwise -> throwError "Binary operator not found"
 where err = "Type not matched between binary operator"
  
analyzeExprF (Ann (BracketExpr expr) (pos, _)) =
  analyzeExprF expr >>= \expr' ->
  return $ Ann (BracketExpr expr') (pos, getT expr')

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
                fail ("parse error: at " ++ show (errorPos e) ++ " with file " ++ file)
       Right p -> case evalStateT (analyzeProgramF p) ([], Main) of
                    Left e -> putStr e >> fail ""
                    Right p' -> return p'
