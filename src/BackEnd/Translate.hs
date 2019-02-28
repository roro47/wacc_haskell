module BackEnd.Translate where

import Prelude hiding (LT, EQ, GT, seq)
import Data.List hiding (insert)
import Control.Monad.State.Lazy
import Data.HashMap as HashMap hiding (map)
import Data.Set as Set hiding (map, foldl)
import FrontEnd.AST
import FrontEnd.Parser
import FrontEnd.SemanticAnalyzer
import qualified BackEnd.Frame as Frame
import qualified BackEnd.Temp as Temp
import BackEnd.Assem as Assem
import BackEnd.IR as IR
import BackEnd.Builtin
-- where to put array index bound??

data Access = Access Frame.Frame Frame.Access deriving (Eq, Show)

data EnvEntry = VarEntry Access Type
              | FunEntry Frame.Frame Temp.Label Type
              deriving (Eq, Show)

data Level = Level { levelFrame :: Frame.Frame,
                     varTable :: (HashMap.Map String EnvEntry),
                     funTable :: (HashMap.Map String EnvEntry) } deriving (Eq, Show)

data TranslateState =
  TranslateState { levels :: [Level],
                   dataFrags :: [Frame.Fragment],
                   procFrags :: [Frame.Fragment],
                   tempAlloc :: Temp.TempAllocator,
                   controlLabelAlloc :: Temp.LabelAllocator,
                   dataLabelAlloc :: Temp.LabelAllocator,
                   frameLabelAlloc :: Temp.LabelAllocator,
                   builtInSet :: Set.Set Int }
                deriving (Eq, Show)

data IExp = Ex Exp
          | Nx Stm
          | Cx (Temp.Label -> Temp.Label -> Stm)

instance Show IExp where
  show (Ex e) = "Ex " ++ show e
  show (Nx s) = "Nx " ++ show s

  show (Cx f) = "Cx " ++ show (f "_" "_")

newTranslateState :: TranslateState
newTranslateState =
  TranslateState { levels = [],
                   dataFrags = [],
                   procFrags = [],
                   tempAlloc = Temp.newTempAllocator,
                   controlLabelAlloc = Temp.newLabelAllocator,
                   dataLabelAlloc = Temp.newLabelAllocator,
                   frameLabelAlloc = Temp.newLabelAllocator,
                   builtInSet = Set.empty }


translateFile :: String -> IO Stm
translateFile file = do
  ast <- parseFile file
  ast' <- analyzeAST ast
  let { (stm, s) = runState (translate ast') newTranslateState }
  return (seq ((map (\(Frame.PROC stm _) -> stm) (procFrags s)) ++ [stm]))

translate :: ProgramF () -> State TranslateState Stm
translate program = do
  stm <- translateProgramF program
  return $ cleanStm stm


seq :: [Stm] -> Stm
seq (stm:stms) = SEQ stm (seq stms)
seq [] = NOP

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
  result <- verifyLevels $ levels state
  case result of
    (level:rest) -> do
                    put $ state { levels = rest }
                    return ()
    otherwise -> fail "verify level fails"

getCurrFrame :: State TranslateState Frame.Frame
getCurrFrame = do
  state <- get
  (level:rest) <- verifyLevels $ levels state
  return $ levelFrame level

addBuiltIn :: [Int] -> State TranslateState ()
addBuiltIn (i:is) = do
  state <- get
  put $ state { builtInSet = Set.insert i (builtInSet state) }
  addBuiltIn is

addBuiltIn [] = return ()

allocLocal :: String -> Type -> Bool -> State TranslateState Access
allocLocal symbol t escape = do
  state <- get
  result <- verifyLevels $ levels state
  case result of
    (level:rest) -> do
      let { frame  = levelFrame level;
            alloc = tempAlloc state;
            (frame', access, alloc') = Frame.allocLocal frame t escape alloc;
            level' = level { levelFrame = frame' }}
      put $ state { levels = (level':rest), tempAlloc = alloc' }
      return $ Access frame access
    otherwise -> fail "verify level fails"


addVarEntry :: String -> Type -> Access -> State TranslateState ()
addVarEntry symbol t access = do
  state <- get
  result <- verifyLevels $ levels state
  case result of
    (level:rest) -> do
      let { varEntry = VarEntry access t;
            level' = level { varTable = HashMap.insert symbol varEntry (varTable level)}}
      put $ state { levels = (level':rest) }
      return ()
    otherwise -> fail "verify level fails"


addFunEntry :: String -> Type -> State TranslateState ()
addFunEntry symbol t = do
  state <- get
  result <- verifyLevels $ levels state
  case result of
    (level:rest) -> do
      let { funEntry = FunEntry (levelFrame level) symbol t;
            level' = level { funTable = HashMap.insert symbol funEntry (funTable level)}}
      put $ state { levels = (level':rest) }
      return ()
    otherwise -> fail "verify level fails"


addFragment :: Frame.Fragment -> State TranslateState ()
addFragment frag = do
  state <- get
  case frag of
    Frame.STRING _ _ _ -> put $ state { dataFrags = frag:(dataFrags state) }
    Frame.PROC _ _ -> put $ state { procFrags = frag:(procFrags state) }

-- obtain how to access a variable
-- TODO : need more modification to make it look better
getVarEntry :: String -> State TranslateState Exp
getVarEntry symbol = do
  state <- get
  (VarEntry (Access frame access) t) <- find' (levels state)
  let frametotal = Frame.frameSize (levelFrame (head (levels state)))
  case access of
    Frame.InReg temp -> return $ TEMP temp
    Frame.InFrame offset -> do
      let { prevLevels = takeWhile notFound (levels state);
            prevSize = sum ( map (\l -> Frame.frameSize (levelFrame l)) prevLevels) +
                       Frame.frameSize ((levelFrame ((levels state) !! (length prevLevels))));
            offset' = foldl f offset prevLevels }
      return $ CALL (NAME "#memaccess") [(CONSTI $  (prevSize - abs offset'))]

  where find' :: [Level] -> State TranslateState EnvEntry
        find' levels =
          case find (\l -> not $ notFound l) levels of
            Just level -> return $ (varTable level) ! symbol
            otherwise -> fail "not found expected var entry"

        notFound level =
          case HashMap.lookup symbol (varTable level) of
            Just (VarEntry _ _) -> False
            otherwise -> True
        f :: Int -> Level -> Int
        f offset level = offset + Frame.frameSize (levelFrame level)

-- adjust stack pointer on return of a function
-- removing all the local variables on the stack
adjustSP :: State TranslateState Stm
adjustSP = do
  state <- get
  let offset = Frame.frameSize (levelFrame (head (levels state)))
  if offset == 0
  then return NOP
  else return $ MOV (TEMP Frame.sp) (BINEXP PLUS (TEMP Frame.sp) (CONSTI offset))

stripParam :: ParamF () -> (Type, String)
stripParam (Ann (Param t (Ann (Ident s) _)) _) = (t,s)

translateFuncF :: FuncF () -> State TranslateState ()
translateFuncF (Ann (Func t id ps stm) _) = do
  let params = reverse $ map stripParam ps
  level <- newLevel
  pushLevel level
  foldM addParam 0 params
  addFunEntry symbol t
  stm' <- translateStatListF stm >>= \s -> unNx s
  adjustSP' <- adjustSP
  let stm'' = SEQ (IR.PUSH (TEMP Frame.lr)) (SEQ stm' (SEQ adjustSP' (POP (TEMP Frame.pc))))
  popLevel
  frame <- getCurrFrame
  addFragment (Frame.PROC (SEQ (LABEL ("f_" ++ symbol)) stm'') frame)
  where Ann (Ident symbol) _ = id

        addParam :: Int -> (Type, String) -> State TranslateState Int
        addParam offset (t, s) = do
          frame <- getCurrFrame
          addVarEntry s t (Access frame (Frame.InFrame $ offset+4))
          return (offset + 4 + Frame.typeSize t)

translateProgramF :: ProgramF () -> State TranslateState Stm
translateProgramF (Ann (Program fs stms) _) = do
  level <- newLevel
  pushLevel level
  mapM translateFuncF fs
  stm <- translateStatListF stms
  stm' <- unNx stm
  state <- get
  adjustSP' <- adjustSP
  popLevel
  return $ SEQ (LABEL "main") (SEQ (SEQ (IR.PUSH (TEMP Frame.lr)) stm')
               (SEQ adjustSP' (SEQ (MOV (TEMP 0) (MEM (CONSTI 0) 4))
               (IR.POP (TEMP Frame.pc)))))


translateStatListF :: StatListF () -> State TranslateState IExp
translateStatListF (Ann (StatList stms) _) = do
  stms' <- mapM translateStatF stms
  stm <- mapM unNx stms'
  return $ Nx (seq stm)

translateStatF :: StatF () -> State TranslateState IExp
translateStatF (Ann (Declare t id expr) _) = do
  let { Ann (Ident symbol) _ = id }
  access <- allocLocal symbol t True
  exp <- translateExprF expr
  let { mem' = MEM (TEMP Frame.sp) (typeLen t)} -- access through sp --use this one!
  addVarEntry symbol t access
  exp' <- unEx exp
  return $ Nx (SEQ adjustSP (MOV mem' exp'))
  where adjustSP =
          MOV (TEMP Frame.sp) (BINEXP MINUS (TEMP Frame.sp) (CONSTI $ Frame.typeSize t))

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
  return $ Nx (EXP (Frame.externalCall "exit" [exp']))

translateStatF (Ann (If expr stms1 stms2) _) = do
  exp <- translateExprF expr
  stms1' <- translateStatListF stms1
  stms2' <- translateStatListF stms2
  c <- unCx exp
  stm1 <- unNx stms1'
  stm2 <- unNx stms2'
  label1 <- newControlLabel
  label2 <- newControlLabel
  label3 <- newControlLabel
  return $ Nx (seq [c label1 label2,
                    LABEL label1, stm1,
                    JUMP (CONSTI 1) [label3],
                    LABEL label2, stm2,
                    LABEL label3])

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
  Nx stms' <- translateStatListF stms
  adjustSP' <- adjustSP
  popLevel
  return $ Nx (SEQ stms' adjustSP')

translateStatF (Ann (FuncStat f) _) = do
  f' <- translateFuncAppF f
  f'' <- unNx f'
  return $ Nx f''

translateExprF :: ExprF () -> State TranslateState IExp
translateExprF (Ann (IntLiter i) _) = return $ Ex (CONSTI i)
translateExprF (Ann (BoolLiter b) _) =
  case b of
    True -> return $ Ex (CONSTI 1)
    False -> return $ Ex (CONSTI 0)
translateExprF (Ann (CharLiter c) _) = return $ Ex (CONSTC c)
translateExprF (Ann (StringLiter s) _) = do
  label <- newDataLabel
  addFragment $ Frame.STRING label ("\"" ++ s ++ "\"") (length s)
  return $ Ex (NAME label)

translateExprF (Ann (ArrayElem (Ann (Ident id) _) exps) (_ , t)) = do
  i <- getVarEntry id
  e <- mapM translateExprF exps
  e' <- mapM unEx e
  addBuiltIn id_p_check_array_bounds
 -- typeLen
  return $ Ex $ MEM (CALL (NAME "#arrayelem") ((CONSTI 4):i:e')) (typeLen t)

-- need to call system function to allocate memory
translateExprF (Ann (ArrayLiter exprs) (_, t)) = do
  exps <- mapM translateExprF exprs
  exps' <- mapM unEx exps
  temp <- newTemp
  let { arrayLen = length exprs;
        (TArray elemT) = t;
        elemSize = Frame.typeSize elemT;
        call = Frame.externalCall "malloc" [CONSTI (arrayLen*elemSize + Frame.intSize), TEMP temp];
        moveElem = f (TEMP temp) 0 elemSize ([CONSTI arrayLen] ++ exps') }
  return $ Ex (ESEQ (SEQ (EXP call) moveElem) (TEMP temp))
  where TArray t' = t
        f temp index elemSize [exp]
          = MOV (MEM (BINEXP PLUS temp (CONSTI (elemSize * index))) (typeLen t')) exp
        f temp index elemSize (exp:exps)
          = SEQ (MOV (MEM (BINEXP PLUS temp (CONSTI (elemSize * index))) (typeLen t')) exp)
                (f temp (index+1) elemSize exps)
        arrayLen = length exprs

translateExprF (Ann (BracketExpr expr) _) = translateExprF expr
translateExprF (Ann (IdentExpr id) (_, t)) = do
  let { Ann (Ident symbol) _ = id }
  exp <- getVarEntry symbol  -- add memory access
  return $ Ex $ MEM exp (typeLen t)

translateExprF (Ann (FuncExpr f) _) = translateFuncAppF f
translateExprF (Ann Null _) = return $ Ex $ (CONSTI 0)


translateFuncAppF :: FuncAppF () -> State TranslateState IExp
translateFuncAppF f@(Ann (FuncApp t id exprs) _) = do
  let { Ann (Ident symbol) _ = id }
  if elem symbol (map fst builtInFunc)
  then translateBuiltInFuncAppF f
  else do
    exps <- mapM translateExprF exprs
    exps' <- mapM unEx exps
    return $ Ex (CALL (NAME symbol) exps')

-- all parameters of user defined function is pushed on stack
{-
translateUserFuncAppF :: FuncAppF () -> State TranslateState IExp
translateUserFuncAppF f@(Ann (FuncApp funcT id exprs) _) = do
  exps <- mapM translateExprF exprs
  exps' <- mapM unEx exps >>= \es -> return $ reverse exps'
  let pushParams = seq $ map pushParam (zip paramTs exps')
      adjustSP' = MOV (TEMP Frame.sp) (BINEXP MINUS (TEMP Frame.sp) (CONSTI totalParamSize))
  return SEQ pushParams (SEQ (CALL symbol exps') adjustSP')


  where Ann (Ident symbol) _ = id
        TFunc _ ps returnT = funcT
        paramTs = map fst $ map stripParam ps -- parameter types
        pushParam prevPush (t, exp) =
          SEQ (MOV (TEMP Frame.sp) (BINEXP MINUS (TEMP Frame.sp) (CONSTI (typeSize t))))
              (MOV (MEM (TEMP Frame.sp) exp))
        totalParamSize = sum $ map typeSize paramTs

 -}

translateBuiltInFuncAppF :: FuncAppF () -> State TranslateState IExp
translateBuiltInFuncAppF (Ann (FuncApp t id exprs) _) = do
  exps <- mapM translateExprF exprs
  exps' <- mapM unEx exps
  let { Ann (Ident symbol) _ = id }
  case symbol of
    "*" -> do {addBuiltIn id_p_throw_overflow_error ;return $ binexp MUL exps'}
    "/" -> do {addBuiltIn id_p_check_divide_by_zero ;return $ binexp DIV exps'}
    "%" -> do {addBuiltIn id_p_check_divide_by_zero ;return $ binexp MOD exps'}
    "+" -> do {addBuiltIn id_p_throw_overflow_error ;return $ binexp PLUS exps'}
    "-" -> do {addBuiltIn id_p_throw_overflow_error ;return $ binexp MINUS exps'}
    "&&" -> return $ binexp AND exps'
    "||" -> return $ binexp OR exps'
    ">" -> return $ condition GT exps'
    ">=" -> return $ condition GE exps'
    "<" -> return $ condition LT exps'
    "<=" -> return $ condition LE exps'
    "==" -> return $ condition EQ exps'
    "!=" -> return $ condition NE exps'
    "skip" -> return $ Nx NOP
    "read" -> do { e <- translateRead (head inputTs) exps';
                   e' <- unEx e;
                   return $ Nx (EXP e') }
    "free" -> do { e <- translateFree (head inputTs) exps';
                   e' <- unEx e;
                   return $ Nx (EXP e') }
    "print" -> translatePrint (head inputTs) exps'
    "println" -> translatePrintln (head inputTs) exps'
    "newpair" -> translateNewPair (TPair (inputTs !! 0) (inputTs !! 1)) exps'
    "fst" -> translatePairAccess ret exps' "fst"
    "snd" -> translatePairAccess ret exps' "snd"
    "!" -> callp "#!" exps'
    "#pos" -> return $ Ex (head exps')
    "#neg" -> do
      case head exps' of
        CONSTI n -> return $ Ex (CONSTI $ -n)
        otherwise -> callp "#neg" exps'
    "len" -> callp "#len" exps'
    "ord" -> callp "#retVal" exps'
    "chr" -> callp "#retVal" exps'
    otherwise -> fail "not predicted situation"
 where (TFunc _ inputTs _) = t
       (TFunc  _ _ ret ) = t
       binexp bop exps =
         let { exp1 = exps !! 0 ; exp2 = exps !! 1 } in
           Ex $ BINEXP bop exp1 exp2
       condition rop exps =
         let { exp1 = exps !! 0 ; exp2 = exps !! 1 } in
          Cx (\label1 label2 -> CJUMP rop exp1 exp2 label1 label2)

callp = \s -> (\exprs -> return $ Ex $ CALL (NAME s) exprs)


translateFree :: Type -> [Exp] -> State TranslateState IExp
translateFree (TPair _ _) exprs = do
  addBuiltIn id_p_free_pair
  callp "#p_free_pair" exprs
translateFree (TArray _) exprs = callp "#p_free_array" exprs
translateFree TStr exprs = callp "#p_free_array" exprs


translateRead :: Type -> [Exp] -> State TranslateState IExp
translateRead TInt exps = do
  addBuiltIn id_p_read_int
  callp "#p_read_int" exps
translateRead TChar exps = do
  addBuiltIn id_p_read_char
  callp "#p_read_char" exps

translatePrint :: Type -> [Exp] -> State TranslateState IExp
translatePrint TChar exps = callp "#p_putchar" exps
translatePrint TInt exps = do
  addBuiltIn id_p_print_int
  callp "#p_print_int" exps
translatePrint TBool exps = do
  addBuiltIn id_p_print_bool
  callp "#p_print_bool" exps
translatePrint TStr exps = do
  addBuiltIn id_p_print_string
  callp "#p_print_string" exps
translatePrint (TArray TChar) exps = callp "#p_print_string" exps
translatePrint (TArray TInt) exps = do
  addBuiltIn id_p_print_int
  callp "#p_print_int" exps
translatePrint t exps = do
  addBuiltIn id_p_print_reference
  callp "#p_print_reference" exps
-- Array & Pair

translatePrintln :: Type -> [Exp] -> State TranslateState IExp
translatePrintln t exps = do
  print <- translatePrint t exps
  print' <- unEx print
  addBuiltIn id_p_print_ln
  return $ Nx (SEQ (EXP print') (EXP (CALL (NAME "#p_print_ln") [])))

{-
 BL MALLOC required here:
 take R0 as a parameter of malloc size
 return the address of malloc in R0
-}
--nil frag?? -- no, handled by qemu
-- treat null as MEM 0
-- null is of type void

show' = (Prelude.filter (/= ' ')).show
translateNewPair :: Type -> [Exp] -> State TranslateState IExp
-- ASSUME 2 parameters
translateNewPair (TPair t1 t2) exps
  = return $ Ex $ CALL (NAME $ "#newpair " ++ (show' t1) ++" "++(show' t2)) exps

translatePairAccess :: Type -> [Exp] -> String -> State TranslateState IExp
translatePairAccess t exps str = do
  addBuiltIn id_p_check_null_pointer
  return $ Ex $ MEM (CALL (NAME ("#" ++ str)) exps) (typeLen t)

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

id_p_print_ln = [0] --0
id_p_print_int = [1] --1
id_p_print_bool = [2] --2
id_p_print_string = [3] --3
id_p_print_reference = [4] --4
id_p_check_null_pointer = [5] ++ id_p_throw_runtime_error --5
id_p_throw_runtime_error = [6] ++ id_p_print_string --6
id_p_read_int = [7] --7
id_p_read_char = [8] --8
id_p_free_pair = [9] ++ id_p_throw_runtime_error --9
id_p_check_array_bounds = [10] ++ id_p_throw_runtime_error --10
id_p_throw_overflow_error = [11] ++ id_p_throw_runtime_error --11
id_p_check_divide_by_zero = [12] ++ id_p_throw_runtime_error --12
