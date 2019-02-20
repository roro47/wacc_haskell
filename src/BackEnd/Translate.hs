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

-- For lily and audrey

-- can not be type here need an expr
translateLen :: Exp -> State TranslateState IExp
-- assume n is msg_ or any array address
translateLen arr = do
  reg <- newTemp
  let temp = TEMP reg in
    return (Nx $ SEQ (MOV temp (MEM arr)) (MOV temp (MEM temp)))

translateChr :: Exp -> State TranslateState IExp
translateChr t@(TEMP temp) = do
  return (Nx $ PUSH t)
translateChr m = do
  reg <- newTemp
  return (Nx $ SEQ (MOV (TEMP reg) m) (PUSH (TEMP reg))) -- memory or int

-- not a fraction
translateOrd :: Exp -> State TranslateState (IExp, Int)
--pre: e is (CONST Char)
translateOrd e = do
  reg <- newTemp
  return ((Nx $ MOV (TEMP reg) e), 1) --STRB hence need to record length

translateFst :: Exp -> Type -> State TranslateState (IExp, Int)
translateFst e (TPair t1 t2) = do
  reg <- newTemp
  let temp = TEMP reg in
    return  (Nx $ SEQ (MOV temp (MEM e)) (MOV temp (MEM temp)), typeLen t1)

translateSnd :: Exp -> Type -> State TranslateState (IExp, Int)
translateSnd e (TPair t1 t2) = do
  reg <- newTemp
  let temp = TEMP reg
      sndPos = BINEXP PLUS temp (CONSTI 4) in
        return ( Nx $ SEQ (MOV temp (MEM e)) (MOV temp (MEM sndPos)), typeLen t2)


translateFree :: Exp -> State TranslateState IExp
-- string & array: need no fragmentation
translateFree e = do
  reg <- newTemp
  return (Nx $ SEQ (MOV (TEMP reg) (MEM e)) (JUMP (NAME "free") ["free"]))

-- for built-in function below, need to generate code and
-- add them to segment list
-- 1. generate function
-- 2. add to func segment list

-- ***NOT INCLUDING PUSH POP***
--pre: FRAGMENT TRANSLATES CAN ONLY BE CALLED AFTER VISITING THE WHOLE TREE
-- AND CAN ONLY BE CALLED ONCE EACH

translateRead :: Type -> State TranslateState ()
translateRead t = do
  msg <- newDataLabel
  temp0 <- newTemp
  temp1 <- newTemp
  let  str TInt = "%d\0"
       str TChar = "%c\0" in
       addFragment (Frame.STRING msg (str t))
  let  statement = SEQ (MOV reg1 reg0) (SEQ (MOV reg0 (NAME msg))
                  (SEQ reg0_plus_4 (JUMP (NAME "scanf") ["scanf"])))
       reg0 = TEMP temp0
       reg1 = TEMP temp1
       reg0_plus_4 = MOV reg0 (BINEXP PLUS reg0 (CONSTI 4))
       f TInt = Frame.newFrame "p_read_int"
       f TChar = Frame.newFrame "p_read_char" in
        addFragment (Frame.PROC statement (f t))


translateFreePair :: State TranslateState ()
-- pair: new fragment
-- pre: Includes throw run time error and print
translateFreePair = do
  msg <- newDataLabel
  temp <- newTemp
  addFragment (Frame.STRING msg "NullReferenceError: dereference a null reference\n\0")
  let  reg0 = TEMP temp
       frame = Frame.newFrame "p_free_pair"
       error_label = "p_throw_runtime_error"
       statement = SEQ (LABEL "free") (SEQ (PUSH reg0) free_all)
       check_null_ptr = SEQ (CJUMP EQ (CONSTI 0) (reg0) "error" "free") run_error
       run_error = SEQ (LABEL "error") (SEQ (MOV reg0 (NAME msg)) (JUMP (NAME error_label) [error_label]))
       free1 = SEQ (MOV (reg0) (MEM reg0)) bl_free
       free2 = SEQ (POP reg0) (SEQ (MOV reg0 (BINEXP PLUS reg0 (CONSTI 4))) bl_free)
       free_all = SEQ free1 (SEQ free2 (SEQ (POP reg0) bl_free))
       bl_free = JUMP (NAME "free") ["free"] in
          addFragment (Frame.PROC statement frame)


translatePrint :: Type -> State TranslateState ()
translatePrint TStr = do
  temp0 <- newTemp
  temp1 <- newTemp
  temp2 <- newTemp
  msg <- newDataLabel
  addFragment (Frame.STRING msg "%.*s\0")
  let  reg0 = TEMP temp0
       reg1 = TEMP temp1
       reg2 = TEMP temp2
       frame = Frame.newFrame "p_print_string"
       s1 = MOV reg1 (MEM reg0)
       s2 = MOV reg2 (BINEXP PLUS reg0 (CONSTI 4))
       s3 = MOV reg0 (NAME msg)
       s4 = MOV reg0 (BINEXP PLUS reg0 (CONSTI 4))
       s5 = JUMP (NAME "printf") ["printf"]
       s6 = MOV reg0 (CONSTI 0)
       s7 = JUMP (NAME "fflush") ["fflush"]
       statement = SEQ s1 (SEQ s2 (SEQ s3 (SEQ s4 (SEQ s5 (SEQ s6 s7))))) in
         addFragment (Frame.PROC statement frame)

translatePrint TChar = translatePrint TStr

translatePrint TInt = do
 msg <- newDataLabel
 temp0 <- newTemp
 temp1 <- newTemp
 addFragment (Frame.STRING msg "%d\0")
 let reg0 = TEMP temp1
     reg1 = TEMP temp1
     frame = Frame.newFrame "p_print_int"
     s1 = MOV reg1 reg0
     s2 = MOV reg0 (NAME msg)
     s3 = MOV reg0 (BINEXP PLUS reg0 (CONSTI 4))
     s4 = JUMP (NAME "printf") ["printf"]
     s5 = MOV reg0 (CONSTI 0)
     s6 = JUMP (NAME "fflush") ["fflush"]
     statement = SEQ s1 (SEQ s2 (SEQ s3 (SEQ s4 (SEQ s5 s6)))) in
        addFragment (Frame.PROC statement frame)

translatePrint TBool = do
  msg0 <- newDataLabel
  msg1 <- newDataLabel
  temp1 <- newTemp
  addFragment (Frame.STRING msg0 "false\0")
  addFragment (Frame.STRING msg1 "true\0")
  let reg0 = TEMP temp1
      frame = Frame.newFrame "p_print_bool"
      s1 = CJUMP NE reg0 (CONSTI 0) "ne" "eq"
      s_ne = SEQ (LABEL "ne") (MOV reg0 (NAME msg0))
      s_eq = SEQ (LABEL "eq") (MOV reg0 (NAME msg1))
      s2 = MOV reg0 (BINEXP PLUS reg0 (CONSTI 4))
      s3 = JUMP (NAME "printf") ["printf"]
      s4 = MOV reg0 (CONSTI 0)
      s5 = JUMP (NAME "fflush") ["fflush"]
      statement = SEQ (SEQ s1 (SEQ s_ne s_eq)) (SEQ s2 (SEQ s3 (SEQ s4 s5))) in
        addFragment (Frame.PROC statement frame)

translatePrintln :: Type -> State TranslateState ()
translatePrintln _ = do
 msg <- newDataLabel
 temp <- newTemp
 addFragment (Frame.STRING msg "\0")
 let reg0 = TEMP temp
     frame = Frame.newFrame "p_print_ln"
     s1 = MOV reg0 (NAME msg)
     s2 = MOV reg0 (BINEXP PLUS reg0 (CONSTI 4))
     s3 = JUMP (NAME "puts") ["puts"]
     s4 = MOV reg0 (CONSTI 0)
     s5 = JUMP (NAME "fflush") ["fflush"]
     statement = SEQ s1 (SEQ s2 (SEQ s3 (SEQ s4 s5))) in
       addFragment (Frame.PROC statement frame)

data ErrorType = RunTime | Overflow | ArrayBound

translateError :: ErrorType -> State TranslateState ()

--   p_throw_runtime_error:
-- 70		BL p_print_string
-- 71		MOV r0, #-1
-- 72		BL exit
translateError RunTime = do
 temp <- newTemp

 let reg0 = TEMP temp
     frame = Frame.newFrame "p_throw_runtime_error:\n\0"
     s1 = JUMP (NAME "p_print_string") ["p_print_string"]
     s2 = MOV reg0 (CONSTI (-1))
     s_exit = JUMP (NAME "exit") ["exit"]
     statement = SEQ (SEQ s1 s2) s_exit in
       addFragment (Frame.PROC statement frame)


 -- p_throw_overflow_error:
 -- 67		LDR r0, =msg_2
 -- 68		BL p_throw_runtime_error
translateError Overflow = do
 msg <- newDataLabel
 temp <- newTemp
 addFragment (Frame.STRING msg "OverflowError: the result is too small/large to store in a 4-byte signed-integer.\n\0")

 let reg0 = TEMP temp
     frame = Frame.newFrame "p_throw_overflow_error:\n\0"
     s1 = MOV reg0 (NAME msg)
     runTimeError = JUMP (NAME "p_throw_runtime_error:\n\0") ["p_throw_runtime_error:\n\0"]
     statement = SEQ s1 runTimeError in
       addFragment (Frame.PROC statement frame)

-- p_check_array_bounds:
-- 67		PUSH {lr}
-- 68		CMP r0, #0
-- 69		LDRLT r0, =msg_0
-- 70		BLLT p_throw_runtime_error
-- 71		LDR r1, [r1]
-- 72		CMP r0, r1
-- 73		LDRCS r0, =msg_1
-- 74		BLCS p_throw_runtime_error
-- 75		POP {pc}

-- translateError ArrayBound = do
--  msg0 <- newDataLabel
--  msg1 <- newDataLabel
--  temp0 <- newTemp
--  temp1 <- newTemp
--  addFragment (Frame.STRING msg0 "ArrayIndexOutOfBoundsError: negative index\n\0")
--  addFragment (Frame.STRING msg1 "ArrayIndexOutOfBoundsError: index too large\n\0")
--
--  let reg0 = TEMP temp0
--      reg1 = TEMP temp1
--      frame = Frame.newFrame "p_check_array_bounds:\n\0"
--      s1 = CJUMP LT reg0 (CONSTI 0) "neg" "rest"
--      s_neg = SEQ (SEQ (LABEL "neg") (MOV reg0 (NAME msg0))) runTimeError
--      s_rest = SEQ (MOV reg1 (MEM reg1)) check_out_of_bound
--      check_out_of_bound =
--      runTimeError = JUMP (NAME "p_throw_runtime_error:\n\0") ["p_throw_runtime_error:\n\0"]
--      statment =  in
--        addFragment (Frame.PROC statement frame)
