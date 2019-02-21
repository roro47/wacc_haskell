module BackEnd.Translate where

import Prelude hiding (LT, EQ, GT, seq)
import Control.Monad.State.Lazy
import Data.HashMap as HashMap hiding (map)
import FrontEnd.AST
import qualified BackEnd.Frame as Frame
import qualified BackEnd.Temp as Temp
import BackEnd.IR


data Access = Access Frame.Frame Frame.Access deriving (Eq, Show)

data EnvEntry = VarEntry Access Type
              | FunEntry Frame.Frame Temp.Label Type
              deriving (Eq, Show)

data Level = Level { levelFrame :: Frame.Frame,
                     varTable :: (HashMap.Map String EnvEntry),
                     funTable :: (HashMap.Map String EnvEntry) } deriving (Eq, Show)

data TranslateState = TranslateState { levels :: [Level],
                                       dataFrags :: [Frame.Fragment],
                                       procFrags :: [Frame.Fragment],
                                       tempAlloc :: Temp.TempAllocator,
                                       controlLabelAlloc :: Temp.LabelAllocator,
                                       dataLabelAlloc :: Temp.LabelAllocator,
                                       frameLabelAlloc :: Temp.LabelAllocator}
                      deriving (Eq, Show)

data IExp = Ex Exp
          | Nx Stm
          | Cx (Temp.Label -> Temp.Label -> Stm)


seq :: [Stm] -> Stm
seq (stm:stms) = SEQ stm (seq stms)
seq [] = MOV (TEMP Frame.rv) (TEMP Frame.rv) 

verifyLevels :: [Level] -> State TranslateState [Level]
verifyLevels [] = fail "no frames available"
verifyLevels levels  = return levels

newTemp :: State TranslateState Temp.Temp
newTemp = do
  state <- get
  let { (tempAlloc', temp) = Temp.newTemp (tempAlloc state) }
  put $ state { tempAlloc = tempAlloc' }
  return temp

newFrameLabel :: State TranslateState Temp.Label
newFrameLabel = do
  state <- get
  let { (alloc, label) = Temp.newFrameLabel (frameLabelAlloc state) }
  put $ state { frameLabelAlloc = alloc }
  return label

newDataLabel :: State TranslateState Temp.Label
newDataLabel = do
  state <- get
  let { (alloc, label) = Temp.newDataLabel (dataLabelAlloc state) }
  put $ state { dataLabelAlloc = alloc }
  return label

newControlLabel :: State TranslateState Temp.Label
newControlLabel = do
  state <- get
  let { (alloc, label) = Temp.newControlLabel (controlLabelAlloc state) }
  put $ state { controlLabelAlloc = alloc }
  return label

newLevel :: State TranslateState Level
newLevel = do
  label <- newFrameLabel
  return $ Level (Frame.newFrame label) HashMap.empty HashMap.empty

pushLevel :: Level -> State TranslateState ()
pushLevel level = do
  state <- get
  put $ state { levels = (level:(levels state)) }
  return ()

popLevel :: State TranslateState ()
popLevel = do
  state <- get
  (level:rest) <- verifyLevels $ levels state
  put $ state { levels = rest }
  return ()


allocLocal :: String -> Type -> Bool -> State TranslateState Access
allocLocal symbol t escape = do
  state <- get
  (level:rest) <- verifyLevels $ levels state
  let { frame  = levelFrame level;
        alloc = tempAlloc state;
        (frame', access, alloc') = Frame.allocLocal frame t escape alloc;
        level' = level { levelFrame = frame' }}
  put $ state { levels = (level':rest), tempAlloc = alloc' }
  return $ Access frame access

addVarEntry :: String -> Type -> Access -> State TranslateState ()
addVarEntry symbol t access = do
  state <- get
  (level:rest) <- verifyLevels $ levels state
  let { varEntry = VarEntry access t;
        level' = level { varTable = insert symbol varEntry (varTable level)}}
  put $ state { levels = (level':rest) }
  return ()

addFunEntry :: String -> Type -> State TranslateState ()
addFunEntry symbol t = do  
  state <- get
  (level:rest) <- verifyLevels $ levels state
  let { funEntry = FunEntry (levelFrame level) symbol t;
        level' = level { funTable = insert symbol funEntry (funTable level)}}
  put $ state { levels = (level':rest) }
  return ()

addFragment :: Frame.Fragment -> State TranslateState ()
addFragment frag = do
  state <- get
  case frag of
    Frame.STRING _ _ -> put $ state { dataFrags = frag:(dataFrags state) }
    Frame.PROC _ _ -> put $ state { procFrags = frag:(procFrags state) }

-- translate access in current frame 
accessToMem :: Access -> Exp
accessToMem (Access _ access) =
  case access of
    Frame.InFrame offset -> MEM (BINEXP PLUS (TEMP Frame.fp) (CONSTI offset))
    Frame.InReg tmp -> TEMP tmp

-- obtain how to access a variable
getVarEntry :: String -> State TranslateState Exp
getVarEntry symbol = do
  state <- get
  let { mem = foldl f (TEMP Frame.fp) (takeWhile notFound $ levels state) }
  return mem
  where notFound level =
          case HashMap.lookup symbol (varTable level) of
            Just (VarEntry _ _) -> False
            otherwise -> True
        f mem level =
          MEM $ BINEXP PLUS (CONSTI $ Frame.frameSize $ levelFrame level) mem


translateProgramF :: ProgramF () -> State TranslateState IExp
translateProgramF (Ann (Program fs stms) _) = translateStatListF stms

translateStatListF :: StatListF () -> State TranslateState IExp
translateStatListF (Ann (StatList stms) _) = do
  stms' <- mapM translateStatF stms
  stm <- mapM unNx stms'
  return $ Nx (seq stm)

translateStatF :: StatF () -> State TranslateState IExp
translateStatF (Ann (Declare t id expr) _) = do
  let { Ann (Ident symbol) _ = id }
  access <- allocLocal symbol t (escape t)
  exp <- translateExprF expr
  let { mem = accessToMem access }
  addVarEntry symbol t access
  exp' <- unEx exp
  return $ Nx (MOV mem exp')

translateStatF (Ann (Assign expr1 expr2) _) = do
  exp1 <- translateExprF expr1
  exp2 <- translateExprF expr2
  exp1' <- unEx exp1
  exp2' <- unEx exp2
  return $ Nx (MOV exp1' exp2')

translateStatF (Ann (Return expr) _) = do
  exp <- translateExprF expr
  exp' <- unEx exp
  return $ Nx (MOV (TEMP Frame.rv) exp')

translateStatF (Ann (Exit expr) _) = do
  exp <- translateExprF expr
  exp' <- unEx exp
  temp <- newTemp
  return $ Nx (MOV (TEMP temp) (Frame.externalCall "exit" [exp']))

translateStatF (Ann (If expr stms1 stms2) _) = do
  exp <- translateExprF expr
  stms1' <- translateStatListF stms1
  stms2' <- translateStatListF stms2
  c <- unCx exp
  stm1 <- unNx stms1'
  stm2 <- unNx stms2'
  label1 <- newControlLabel
  label2 <- newControlLabel
  return $ Nx (seq [c label1 label2,
                    SEQ (LABEL label1) stm1, SEQ (LABEL label2) stm2])

translateStatF (Ann (While expr stms) _) = do
  exp <- translateExprF expr
  stms' <- translateStatListF stms
  c <- unCx exp
  stm <- unNx stms'
  test <- newControlLabel
  body <- newControlLabel
  done <- newControlLabel
  return $ Nx (seq [LABEL test, c body done,
                    LABEL body, stm,
                    JUMP (CONSTI 1) [test],
                    LABEL done])

translateStatF (Ann (Subroutine stms) _) = do
  level <- newLevel
  pushLevel level
  stms' <- translateStatListF stms
  popLevel
  return $ stms'

translateExprF :: ExprF () -> State TranslateState IExp
translateExprF (Ann (IntLiter i) _) = return $ Ex (CONSTI i)
translateExprF (Ann (BoolLiter b) _) =
  case b of
    True -> return $ Ex (CONSTI 1)
    False -> return $ Ex (CONSTI 0)
translateExprF (Ann (CharLiter c) _) = return $ Ex (CONSTC c)
translateExprF (Ann (StringLiter s) _) = do
  label <- newDataLabel
  addFragment $ Frame.STRING label s
  return $ Ex (NAME label)

-- need to call system function to allocate memory
translateExprF (Ann (ArrayLiter exprs) (_, t)) = do
  exps <- mapM translateExprF exprs
  exps' <- mapM unEx exps
  temp <- newTemp
  let { arrayLen = length exprs;
        (TArray elemT) = t;
        elemSize = Frame.typeSize elemT;
        call = Frame.externalCall "malloc" [CONSTI (arrayLen*elemSize)];
        moveElem = f (TEMP temp) 0 elemSize exps' }
  return $ Ex (ESEQ (SEQ (MOV (TEMP temp) call) moveElem) (TEMP temp))
  where f temp index elemSize [exp]
          = MOV (BINEXP PLUS temp (CONSTI (elemSize * index))) exp
        f temp index elemSize (exp:exps)
          = SEQ (MOV (BINEXP PLUS temp (CONSTI (elemSize*index))) exp)
                (f temp (index+1) elemSize exps)
  
translateExprF (Ann (BracketExpr expr) _) = translateExprF expr
translateExprF (Ann (IdentExpr id) _) = do
  let { Ann (Ident symbol) _ = id }
  exp <- getVarEntry symbol
  return $ Ex exp

translateExprF (Ann (FuncExpr f) _) = translateFuncAppF f

translateFuncAppF :: FuncAppF () -> State TranslateState IExp
translateFuncAppF f@(Ann (FuncApp t id exprs) _) = do
  let { Ann (Ident symbol) _ = id }
  if elem symbol (map fst builtInFunc)
  then translateBuiltInFuncAppF f
  else undefined

translateBuiltInFuncAppF :: FuncAppF () -> State TranslateState IExp
translateBuiltInFuncAppF (Ann (FuncApp t id exprs) _) = do
  exps <- mapM translateExprF exprs
  exps' <- mapM unEx exps
  let { Ann (Ident symbol) _ = id }
  case symbol of
    "*" -> return $ binexp MUL exps' 
    "/" -> return $ binexp DIV exps'
    "%" -> return $ binexp MOD exps'
    "+" -> return $ binexp PLUS exps'
    "-" -> return $ binexp MINUS exps'
    "&&" -> return $ binexp AND exps'
    "||" -> return $ binexp OR exps'
    ">" -> return $ condition GT exps'
    ">=" -> return $ condition GE exps'
    "<" -> return $ condition LT exps'
    "<=" -> return $ condition LE exps'
    "==" -> return $ condition EQ exps'
    "!=" -> return $ condition NE exps'
    otherwise -> fail "not predicted situation"
 where binexp bop exps =
         let { exp1 = exps !! 0 ; exp2 = exps !! 1 } in
           Ex $ BINEXP bop exp1 exp2
       condition rop exps =
         let { exp1 = exps !! 0 ; exp2 = exps !! 1 } in
          Cx (\label1 label2 -> CJUMP rop exp1 exp2 label1 label2)

-- turn IExp to Exp
unEx :: IExp -> State TranslateState Exp
unEx (Ex e) = return e
unEx (Cx genStm) = do
  temp <- newTemp
  label1 <- newControlLabel
  label2 <- newControlLabel
  return $ ESEQ (seq [MOV (TEMP temp) (CONSTI 1),
                      genStm label1 label2,
                      LABEL label2,
                      MOV (TEMP temp) (CONSTI 0),
                      LABEL label1]) (TEMP temp)  
unEx (Nx s) = return $ ESEQ s (CONSTI 0)

-- turn IExp to Stm
unNx :: IExp -> State TranslateState Stm
unNx (Nx stm) = return stm
unNx (Ex e) = do
  temp <- newTemp
  return $ MOV (TEMP temp) e
unNx (Cx c) = do
  label1 <- newControlLabel
  label2 <- newControlLabel
  return $ c label1 label2

-- turn IExp to conditionals
unCx :: IExp -> State TranslateState (Temp.Label -> Temp.Label -> Stm)
unCx (Ex e) = do
  case e of
    CONSTI 0 -> return $ (\label1 label2 -> JUMP e [label2])
    CONSTI 1 -> return $ (\label1 label2 -> JUMP e [label1])
    otherwise -> return $ (\label1 label2 -> CJUMP EQ e (CONSTI 1) label1 label2)
    
unCx (Cx c) = return c
unCx (Nx _) = undefined

escape :: Type -> Bool
escape TInt = False
escape TBool = False
escape TStr = False
escape TChar = False
escape _ = True

                      
