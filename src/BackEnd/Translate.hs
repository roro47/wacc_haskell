module BackEnd.Translate where

import Control.Monad.State.Lazy
import Data.HashMap as HashMap hiding (map)
import BackEnd.Frame as Frame
import qualified BackEnd.IR as IR

data Access = Access Frame.Frame Frame.Access

data EnvEntry = VarEntry Access Type
              | FunEntry Frame Temp.Label Type
              deriving (Eq, Show)

-- to avoid clash of mapping to varEntry and funEntry
-- key for funEntry is "%functionName"
data Level = Level Frame.Frame (HashMap.Map String EnvEntry) deriving (Eq, Show)

data TranslateState = TranslateState { levels :: [Level],
                                       dataFrags :: [Fragment],
                                       procFrags :: [Fragment],
                                       tempAllocator :: TempAllocator,
                                       labelAllocator :: LabelAllocator,
                                       }
                      deriving (Eq, Show)

data IExp = Ex IR.Exp
          | Nx IR.Stm
          | Cx (Temp.Label -> Temp.Label -> IR.Stm)
         deriving (Eq, Show)


data ScopeAllocator = ScopeAllocator Int deriving (Eq, Show)

newScopeName' :: ScopeAllocator -> (ScopeAllocator, String)
newScopeName' (ScopeAllocator i) = (ScopeAllocator i+1, show i ++ "_scope")

-- should panic if length stm < 1
seq :: [Stm] -> Stm
seq (stm:stms) = SEQ stm (seq stms)
seq stm = stm


-- turn IExp to Exp
unEx :: IExp -> State TranslateState IR.Exp
unEx (Ex e) = return e
unEx (Cx genStm) = do
  temp <- newTemp
  label1 <- newTemp
  label2 <- newTemp
  return $ ESEQ (seq [MOVE (TEMP temp) (CONSTI 1),
                      genStm label1 label2,
                      LABEL label2,
                      MOVE (TEMP temp) (CONSTI 0),
                      LABEL label1]) (TEMP temp)  
unEx (Nx s) = return $ IR.ESEQ s (IR.CONSTI 0)

-- TODO
unNx :: IExp -> State TranslateState IR.Stm
unNx = undefined

-- TODO
unCx :: IExp -> State TranslateState (Temp.Label -> Temp.Label -> IR.Stm)
unCx (Ex e) = undefined
unCx (Cx c) = return c
unCx (Nx _) = undefined


verifyLevels :: [Level] -> State TranslateState [Level]
verifyLevels [] = fail "no frames available"
verifyLevels levels  = return levels

topFrame :: State TranslateState Frame.Frame
topFrame = undefined

pushLevel :: String -> State TranslateState ()
pushLevel scopeName = do
  state <- get
  put $ state { levels = (Frame scopeName 0, HashMap.empty):levels state }
  return ()

popLevel :: State TranslateState ()
popLevel = do
  state <- get
  (level:rest) <- verifyLevels $ levels state
  put $ state { levels = rest }
  return ()

newTemp :: State TranslateState Temp.Temp
newTemp = do
  state <- get
  let { (tempAlloc, temp) = Temp.newTemp (tempAllocator state) }
  put $ state { tempAllocator = temp }
  return temp

newScopeName :: State TranslateState String
newScopeName = do
  state <- get
  let { (scopeAlloc', name) = newScopeName' (scopeAllocator state) }
  put $ state { scopeAlloc = scopeAlloc' }
  return name

-- allocate a local variable to a frame
allocLocal :: String -> Type -> Bool -> State TranslateState Access
allocLocal symbol t escaped = do
  state <- get
  ((frame, env):rest) <- verifyLevels $ levels state
  let { tmpAlloc = tempAllocator state;
        (frame', access, tmpAlloc') = Frame.allocLocal frame escaped tmpAlloc}
  put $ state { levels = (frame', env):levels, tempAllocator = tmpAlloc'}
  addVarEntry symbol t access
  return ()

newLabel :: State TranslateState Temp.Label
newLabel = do
  state <- get
  let { (labelAlloc', label) = Temp.newLabel $ labelAllocator state }
  put $ state { labelAllocator = labelAlloc' }
  return label

addVarEntry :: String -> Type -> Access -> State TranslateState ()
addVarEntry symbol t access = do
  state <- get
  ((frame, env):rest) <- verifyLevels $ levels state
  let { varEntry = VarEntry access t }
  put $ state { levels = (frame, insert symbol varEntry env):rest }
  return ()

addFunEntry :: String -> Type -> State TranslateState ()
addFunEntry label t = do  
  state <- get
  ((frame, env):rest) <- verifyLevels $ levels state
  let { funEntry = FunEntry frame label t }
  put $ state { levels = (frame, insert ("%" ++ label) funEntry env):rest }
  return ()

addFragment :: Fragment -> State TranslateState ()
addFragment frag = do
  state <- get
  case frag of
    STRING _ _ -> put $ state { dataFrags = frag:(dataFrags state) }
    PROC _ _ -> put $ state { procFrags = frag:(procFrags state) }

-- translate access in current frame 
accessToMem :: Access -> Exp
accessToMem (_, access) =
  case access of
    InFrame offset -> MEM (BINOP PLUS fp (CONSTI offset))
    InReg tmp -> TEMP tmp

-- obtain how to access a variable
getVarEntry :: String -> State TranslateState IExp
getVarEntry symbol = do
  state <- get
  let { mem = foldl f (TEMP Temp.fp) (takeWhile notFound (levels state)) }
  return $ Ex mem
  where notFound (Level _ env) =
          case HashMap.lookup symbol env of
            Just (VarEntry _ _) -> False
            otherwise -> True
        f mem (Level frame _) = MEM $ BINEXP PLUS (frameSize frame) mem

translateProgramF :: ProgramF () -> State TranslateState IExp
translateProgramF (Ann (Program fs stms) _) = undefined

translateFuncF :: FuncF () -> State TranslateState IExp
translateFuncF (Ann (Func t id params stms) _) = undefined

translateStatListF :: StatListF () -> State TranslateState IExp
translateStatListF (Ann (StatList stms) _) = undefined

translateStatF :: StatF () -> State TranslateState IExp
translateStatF (Ann (Declare t id expr) _) = do
  access <- allocLocal
  exp <- translateExprF expr
  let { mem = accessToMem tmp;
        Ann (Ident symbol) _  = id }
  addVarEntry symbol t access
  return $ Nx (MOV mem (unEx exp))

translateStatF (Ann (Assign expr1 expr2) _) = do
  exp1 <- translateExprF expr1
  exp2 <- translateExprF expr2
  return $ Nx (MOV (unEx exp1) (unEx exp2))

translateStatF (Ann (Return expr) _) = do
  exp <- translateExprF expr
  return $ Nx (MOV Frame.rv exp)

-- TODO: need to call system function
translateStatF (Ann (Exit expr) _) = do
  exp <- translateExprF expr
  return $ Nx (externalCall "exit" [unEx exp])

translateStatF (Ann (If expr stms1 stms2) _) = do
  exp <- translatExprF expr
  stms1' <- translateStatListF stms1
  stms2' <- translateStatListF stms2
  c <- unCx exp
  stm1 <- unNx stms1'
  stm2 <- unNx stms2'
  label1 <- newLabel
  label2 <- newLabel
  return $ Nx (seq [c label1 label2,
                    SEQ (LABEL label1) stm1, SEQ (LABEL label2) stm2])

translateStatF (Ann (While expr stms) _) = do
  exp <- translateExprF expr
  stms' <- translateStatListF stms
  c <- unCx exp
  stm <- unEx stms
  test <- newLabel
  body <- newLabel
  done <- newLabel
  return $ Nx (seq [LABEL test, c body done,
                    LABEL body, stm,
                    JUMP (CONSTI 1, [LABEL test]) -- just have simple jump?
                    LABEL done])

translateStatF (Ann (Subroutine stms) _) = do
  scopeName <- newScopeName
  pushLevel scopeName
  stms' <- translateStatListF stms
  popLevel
  return $ Nx stms'
  
translateExprF :: ExprF () -> State TranslateState IExp
translateExprF (Ann (IntLiter i) _) = return $ CONSTI i
translateExprF (Ann (BoolLiter b) _) =
  case b of
    True -> return $ Ex (CONSTI 1)
    False -> return $ Ex (CONSTI 0)
translateExprF (Ann (CharLiter c) _) = return $ Ex (CONSTC c)
translateExprF (Ann (StringLiter s) _) = do
  label <- newLabel
  addFragment $ STRING label s
  return $ Ex (NAME label) 

-- need to call system function to allocate memory
translateExprF (Ann (ArrayLiter a) _) = undefined
translateExprF (Ann (BracketExpr expr) _) = translateExprF expr
translateExprF (Ann (IdentExpr id) _) =
  let { Ann (Ident symbol) _ = id }
  exp <- getVarEntry symbol
  return Ex exp

translateExprF (Ann (ArrayElem id exprs) (_, t)) = do
  let { Ann (Ident symbol) _ = id }
  a <- getVarEntry symbol -- array address
  exps <- mapM translateExprF exprs
  let { (mem, _) = foldl f (a, t) (map unEx exps) }
  return $ Ex mem
  where f :: (Exp, Type) -> Exp 
        f (mem, TArray t) exp =
          (MEM $ BINOP PLUS mem (BINOP MUL exp addrSize) , t)
        f (mem, t) exp =
          let size = case t of
                         TInt -> intSize
                         TBool -> intSize
                         TChar -> charSize
                         TPair _ _ -> addrSize
                         _ -> intSize  in 
          (MEM $ BINOP PLUS mem (BINOP MUL exp size), t)
            

translateBuiltInFuncAppF :: FuncAppF () -> State TranslateState IExp
translateBuiltInFuncAppF (Ann (FuncApp t id exprs) _) = do
  exps <- map translateExprF exprs
  let { Ann (Ident symbol) _ = id }
  case id of
    "read" -> translateRead
    "free" -> translateFree
    "print" -> translatePrint
    "println" -> translatePrintln
    "len" -> translate
    "*" -> return $ binexp MUL 
    "/" -> return $ binexp DIV 
    "%" -> return $ binexp MOD 
    "+" -> return $ binexp PLUS 
    "-" -> return $ binexp MINUS
    "&&" -> return $ binexp AND
    "||" -> return $ binexp OR
    ">" -> return $ condition GT 
    ">=" -> return $ condition GE 
    "<" -> return $ condition LT 
    "<=" -> return $ condition LE 
    "==" -> return $ condition EQ 
    "!=" -> return $ condition NE
    otherwise -> fail "not predicted situation"
 where binexp bop exps =
         let { exp1 = exps !! 0 ; exp2 = exps !! 1 } in
           Ex $ BINEXP bop exp1 exp2
       condition rop exps =
         let { exp1 = exps !! 0 ; exp2 = exps !! 1 } in
          Ex $ Cx (\label1 label2 -> CJUMP rop exp1 exp2 label1 label2)

translateUserFuncAppF :: FuncAppF () -> State TranslateState IExp
translateUserFuncAppF (Ann (FuncApp id exprs) _) = undefined


 -- For lily and audrey
translateLen :: String -> StateTranslateState IExp
translateLen s = undefined

translateOrd :: Exp -> StateTranslateState IExp
translateOrd e = undefined

translateFst :: Exp -> StateTranslateState IExp
translateFst e = undefined

translateSnd :: Exp -> StateTranslateState IExp
translateSnd e = undefined

-- for built-in function below, need to generate code and
-- add them to segment list
-- 1.generate function
-- 2. add to func segment list
translateRead :: Type -> State TranslateState ()
translateRead = undefined

translateFree :: Type -> State TranslateState ()
translateFree = undefined

translatePrint :: Type -> State TranslateState ()
translatePrint = undefined

translatePrintln :: Type -> State TranslateState ()
translatePrintln = undefined

translateFree :: Type -> State TranslateState ()
translateFree = undefined


