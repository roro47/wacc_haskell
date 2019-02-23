module BackEnd.Munch where
import BackEnd.Instructions as ARM
import BackEnd.IR as IR
import BackEnd.Assem as ASSEM
import BackEnd.Temp hiding (newTemp, newDataLabel)
import Data.Maybe
import Control.Monad.State.Lazy
import BackEnd.Translate as T
import BackEnd.Frame as Frame
import Data.List
import BackEnd.Builtin
import FrontEnd.Parser
import FrontEnd.SemanticAnalyzer
--how to know scope ?? when frame is changed ???
--TODO : difference between b and bl ??
-- REGISTER SAVE??
-- STRING ASSIGNMENT?
optimsedMunch stm = do
  m <- munchStm stm
  let out = optimise (normAssem [(13, SP), (14, LR), (15, PC), (1, R1), (0, R0)] m)
  return $ mapM putStrLn $ zipWith (++) (map (\x -> (show x) ++"  ") [0..]) (map show out)

bopToCBS :: BOp ->  Maybe (Suffix -> Cond -> Calc)
bopToCBS bop
  = lookup bop [(IR.PLUS, ARM.ADD), (IR.AND, ARM.AND), (IR.OR, ARM.ORR),
            (IR.LSHIFT, ARM.LSL), (IR.RSHIFT, ARM.LSR)]

justret e = do
  (i, t) <- munchExp e
  return (i , t)

munchExp :: Exp -> State TranslateState ([ASSEM.Instr], Temp)
munchExp (CALL (NAME "#retVal") [e]) = justret e

munchExp (CALL (NAME "#arrayelem") [(CONSTI size), ident, pos]) = do
  (ii, it) <- munchExp ident
  (pi, pt) <- munchExp pos
  let op = if size == 1 then (R (RTEMP pt)) else (LSL_ (RTEMP pt) 2)
      m0 = move_to_r pt 0
      m1 = move_to_r it 1
      bl = IOPER {assem = BRANCH_ (BL AL) (L_ "p_check_array_bounds"),
                  src = [0, 1], dst = [], jump = ["p_check_array_bounds"]}
      skiplen = IOPER {assem = CBS_ (ADD NoSuffix AL) (RTEMP it) (RTEMP it) (IMM 4),
                       src = [it], dst = [it], jump = []}
      topos = IOPER {assem = CBS_ (ADD NoSuffix AL) (RTEMP it) (RTEMP it) op,
                       src = [it, pt], dst = [it], jump = []}
  return (ii ++ pi ++ [m0, m1, bl, skiplen, topos], it)

munchExp (CALL (NAME "#neg") [e]) = do
  (i, t) <- munchExp e
  let rsbs = IOPER { assem = CBS_ (RSB S AL) (RTEMP t) (RTEMP t) (IMM 0),
                     src = [t], dst = [t], jump = []}
      check = IOPER {assem = BRANCH_ (BL VS) (L_ "p_throw_overflow_error"),
                     src = [], dst = [], jump =["p_throw_overflow_error"] }
  return (i ++ [rsbs, check] , t)

munchExp (CALL (NAME "#!") [e]) = do
  (i, t) <- munchExp e
  return (i ++ [IOPER {assem = CBS_ (EOR NoSuffix AL) (RTEMP t) (RTEMP t)(IMM 1),
                       src = [t], dst = [t], jump = []}], t)

munchExp (CALL (NAME "#len") [e]) = do
  (i, t) <- munchExp e
  return (i ++ [IMOV {assem = S_ (LDR W AL) (RTEMP t) (Imm (RTEMP t) 0),
                       src = [t], dst = [t]}], t)

munchExp (CALL (NAME "#skip") _) = return ([], dummy)

munchExp (CALL (NAME "#println") es) = do
  ls <- mapM (liftM fst.munchExp) es
  let ln = IOPER {assem = BRANCH_ (BL AL) (L_ "#p_print_ln"),
                  src = [], dst = [], jump = ["#p_print_ln"]}
  return ((concat ls)++[ln], dummy)

munchExp (CALL (NAME "#p_putchar") [e]) = do
  (i, t) <- munchExp e
  let mv = move_to_r t 0
      putchar = ljump_to_label "putchar"
  return (i ++ [mv, putchar], dummy)

munchExp (CALL (NAME n) [e])
  | "#p_" `isPrefixOf` n = do
    (i, t) <- munchExp e
    return  (i++ [(move_to_r t 0), (ljump_to_label n)], dummy)

munchExp (CALL (NAME n) e)
  | "#fst" `isPrefixOf` n = accessPair True fst e
  | "#snd" `isPrefixOf` n = accessPair False snd e
  | "#newpair " `isPrefixOf` n = createPair fst snd e
    where
      ls = words n
      fst = (ls !! 1)
      snd = (ls !! 2)

{- r0 / r1 : result in r0
  need to check if r0 is divided by zero-}
munchExp (BINEXP DIV e1 e2) = do
  (i1, t1) <- munchExp e1 -- dividend
  (i2, t2) <- munchExp e2 --divisor
  let divLabel = "__aeabi_idiv"
      moveDividend = move_to_r t1 0
      moveDivisor = move_to_r t2 1
      check = IMOV {assem = BRANCH_ (BL AL) (L_ "p_check_divide_by_zero"),
                    src = [0, 1], dst = []}
      divInstr = IMOV {assem = BRANCH_ (BL AL) (L_ divLabel),
                      src = [0, 1], dst = [0]} in
      return $ (i1 ++ i2 ++ [moveDividend, moveDivisor, divInstr], 0)

{- r0 % r1 : result in r1
  need to check if r0 is divided by zero-}
munchExp (BINEXP MOD e1 e2) = do
  (i1, t1) <- munchExp e1 -- dividend
  (i2, t2) <- munchExp e2  --divisor
  let modLabel = "__aeabi_idivmod"
      moveDividend = move_to_r t1 0
      moveDivisor = move_to_r t2 1
      check = IMOV {assem = BRANCH_ (BL AL) (L_ "p_check_divide_by_zero"),
                    src = [0, 1], dst = []}
      modInstr = IMOV {assem = BRANCH_ (BL AL) (L_ modLabel),
                  src = [0, 1], dst = [1]} in
      return $ (i1 ++ i2 ++ [moveDividend, moveDivisor, modInstr], 1)

{-If munched stm is of length 2 here then it must be a SEQ conaing a naive stm and a label -}
munchExp (ESEQ (SEQ cjump@(CJUMP rop _ _ _ _) (SEQ false true)) e) = do
  cinstr <- munchStm cjump
  state <- get
  falseinstr <- munchStm false
  if (length falseinstr == 2) then
    do
    put state
    let (fCommand, fBranch) = deSeq false
    condf <- condStm fCommand
    branchf <- munchStm fBranch
    state2 <- get
    trueinstr <- munchStm true
    if(length trueinstr == 2) then
      do
      put state2
      condt <- condStm (snd $ deSeq true) -- branch info not needed
      (i, t) <- munchExp e
      return ((condf (invert rop)) ++ (condt $ same rop) ++ i, t)
    else
      do
      (i, t) <- munchExp e
      return ((condf (invert rop)) ++ branchf ++ trueinstr ++ i, t)
  else
    do
    trueinstr <- munchStm true
    (i, t) <- munchExp e
    return (cinstr ++ falseinstr ++ trueinstr ++ i, t)

munchExp (CALL f es) = do
  (fi, ft) <- munchExp f -- assume result returned in ft
  ls <- mapM (liftM fst.munchExp) es
  let returnVal = move_to_r ft 0
  return ((concat ls) ++ fi ++ [returnVal], 0) -- returned in reg 0

-- NO CALLER / CALLEE SAVE CONVENTION YET !!

munchExp (TEMP t) = return ([],t)

munchExp (BINEXP MINUS e1 e2) = do
  (i1, t1) <- munchExp e1
  (i2, t2) <- munchExp e2
  let subs = IOPER {assem = CBS_ (SUB S AL) (RTEMP t1) (RTEMP t1) (R (RTEMP t2)),
                    src = [t1, t2], dst = [t1], jump = []}
      br = IOPER {assem = BRANCH_ (BL VS) (L_ "p_throw_overflow_error"),
                  src = [], dst = [], jump = ["p_throw_overflow_error"]}
  return (i1++i2++[subs, br], t1)

munchExp (BINEXP MUL e1 e2) = do -- only the lower one is used
  (i1, t1) <- munchExp e1
  (i2, t2) <- munchExp e2
  tLo <- newTemp
  tHi <- newTemp
  let smull = IMOV {assem = C2_ (SMULL NoSuffix AL) (RTEMP tLo)
                    (RTEMP tHi) (RTEMP t1) (RTEMP t2),
                    src = [t1, t2], dst = [tLo, tHi]}
      cmp = IOPER {assem = MC_ (CMP AL) (RTEMP tHi) (ASR_ (RTEMP tLo) 31),
                   src = [tHi, tLo], dst = [], jump = []}
      throw = IOPER {assem = BRANCH_ (BL ARM.NE) (L_ "p_throw_overflow_error"),
                   src = [], dst = [], jump = ["p_throw_overflow_error"]}
  return $ (i1 ++ i2 ++ [smull, cmp, throw], tLo)

munchExp x = do
  c <- condExp x
  return $ c AL

lslOP :: Exp -> Exp -> BOp -> Int -> State TranslateState (Cond -> ([ASSEM.Instr], Temp))
lslOP e1 e2 bop int = do
  (i1, t1) <- munchExp e1
  (i2, t2) <- munchExp e2
  return $ \c -> (i1 ++ i2 ++ [IOPER {assem = CBS_ (addsubtoCalc bop $ c) (RTEMP t1) (RTEMP t1)
                  (LSL_ (RTEMP t2) (log2 int)), dst = [t1], src = [t2], jump = []}] , t1)

canlsl bop int = (bop == MINUS || bop == PLUS) && (int == 2 || int == 4 || int == 8)

condExp :: Exp -> State TranslateState (Cond -> ([ASSEM.Instr], Temp))
-- LSL inside ADD SUB  ** ugly pattern match to avoid run time loop --
condExp (BINEXP bop (BINEXP MUL e1 (CONSTI int)) e2)
  | canlsl bop int
      = lslOP e1 e2 bop int

condExp (BINEXP bop e1 (BINEXP MUL (CONSTI int) e2))
  | canlsl bop int
      = lslOP e1 e2 bop int

condExp (BINEXP bop e1 (BINEXP MUL e2 (CONSTI int)))
  | canlsl bop int
      = lslOP e1 e2 bop int

condExp (BINEXP bop (BINEXP MUL (CONSTI int) e1) e2)
  | canlsl bop int
      = lslOP e1 e2 bop int

condExp (BINEXP bop (CONSTI int) e) = condExp (BINEXP bop e (CONSTI int))

condExp (BINEXP bop e (CONSTI int)) = do
  (i1, t1) <- munchExp e
  let {cbs = bopToCBS bop}
  case cbs of
    Nothing -> fail ""
    otherwise -> return $ \c -> (i1 ++ [IOPER {assem = CBS_ ((fromJust cbs) NoSuffix c)
                                (RTEMP t1) (RTEMP t1) (IMM int),
                                dst = [t1], src = [t1], jump = []}], t1)

condExp (BINEXP bop e1 e2) = do
  (i1, t1) <- munchExp e1
  (i2, t2) <- munchExp e2
  let {cbs = bopToCBS bop}
  case cbs of
    Nothing -> fail ""
    otherwise -> return $ \c -> (i1 ++ i2 ++ [IOPER {assem = CBS_ ((fromJust cbs) NoSuffix c)
                                (RTEMP t1) (RTEMP t1) (R $ RTEMP t2),
                                dst = [t1], src = [t1, t2], jump = []}], t1)

condExp (CONSTI int) = do
  t <- newTemp
  return $ \c -> ([IMOV {assem = MC_ (ARM.MOV c) (RTEMP t) (IMM int) , dst = [t], src = []}], t)

condExp (CONSTC chr) = do
  t <- newTemp
  return $ \c -> ([IMOV {assem = S_ (LDR B_ c) (RTEMP t) (CHR_ chr), dst = [t], src = []}], t)

condExp (NAME l) = do
  t <- newTemp
  return $ \c -> ([IMOV {assem = S_ (ARM.LDR W c) (RTEMP t) (MSG l) , dst = [t], src = []}], t)

condExp (MEM (CONSTI i)) = do
  newt <- newTemp
  return $ \c -> ([IMOV {assem = S_ (ARM.LDR W c) (RTEMP newt) (NUM i) , dst = [], src = [newt]}], newt)

--load a byte from sp
condExp (CALL (NAME "#oneByte") [exp]) = do
  (i, t) <- munchExp exp
  newt <- newTemp
  return $ \c -> (i ++ [IMOV {assem = S_ (ARM.LDR SB c) (RTEMP newt) (Imm (RTEMP t) 0)
                        , dst = [t], src = [newt]}], newt)

condExp (MEM m) = do
  (i, t) <- munchExp m
  newt <- newTemp
  return $ \c -> (i ++ [IMOV {assem = S_ (ARM.LDR W c) (RTEMP newt) (Imm (RTEMP t) 0)
                        , dst = [t], src = [newt]}], newt)


addsubtoCalc :: BOp -> (Cond -> Calc)
addsubtoCalc PLUS = (\c -> ARM.ADD NoSuffix c)
addsubtoCalc MINUS = (\c -> ARM.SUB NoSuffix c)

log2 :: Int -> Int
log2 2 = 1
log2 4 = 2
log2 8 = 3

munchMem :: Exp -> State TranslateState ([ASSEM.Instr], [Int], SLOP2)
--- PRE-INDEX ---
-- HANDLED USING optimise

-- TODO: more simplification allowed here : eval the expression if possible to a int....
---- IMMEDIATE ----
munchMem (TEMP t) = return ([], [t], Imm (RTEMP t) 0)
munchMem (BINEXP PLUS (TEMP t) (CONSTI int)) = return ([], [t], Imm (RTEMP t) int)
munchMem (BINEXP PLUS (CONSTI int) (TEMP t)) = return ([], [t], Imm (RTEMP t) int)
munchMem (CONSTI int) = return ([], [], NUM int)

--- ALL OTHER CASES ---
{- Including msg -}
munchMem e = do
  (i, t) <- munchExp e
  return (i, [t], MSG "SLOP2 NOT USED")

--- CAUTION : NEED TO TEST THE IMM OFFSET RANGE OF THE TARGET MACHINE ---
optimise :: [ASSEM.Instr] -> [ASSEM.Instr]
-- PRE-INDEX --
optimise ((IOPER { assem = (CBS_ c (RTEMP t11) (RTEMP t12) (IMM int))}) :
          (IMOV { assem = (S_ sl (RTEMP t21) (Imm (RTEMP t22) 0))}) :remain)
  | (stackEqualCond c sl) && t11 == t12 && t22 == t11
        = IMOV { assem = (S_ sl (RTEMP t21) (PRE (RTEMP t11) (opVal c * int))),src = [t11], dst = [t12]}
                : optimise remain
optimise (x:xs) = x : (optimise xs)
optimise [] = []

stackEqualCond :: Calc -> SL -> Bool
stackEqualCond (ARM.ADD _ c1) (LDR _ c2) = c1 == c2
stackEqualCond (ARM.ADD _ c1) (STR _ c2) = c1 == c2
stackEqualCond (ARM.SUB _ c1) (LDR _ c2) = c1 == c2
stackEqualCond (ARM.SUB _ c1) (STR _ c2) = c1 == c2
stackEqualCond _ _ = False

opVal :: Calc -> Int
opVal (ARM.ADD _ _) = 1
opVal _ = -1

munchStm :: Stm -> State TranslateState [ASSEM.Instr] -- everything with out condition

munchStm (LABEL label) = return [ILABEL {assem = LAB label, lab = label}]

munchStm (SEQ s1 s2) = do
  l1 <- munchStm s1
  l2 <- munchStm s2
  return $ l1 ++ l2

munchStm (CJUMP rop e1 e2 t f) = do -- ASSUME CANONICAL
  (i1, t1) <- munchExp e1
  (i2, t2) <- munchExp e2
  let compare = IOPER {assem = MC_ (ARM.CMP AL) (RTEMP t1) (R (RTEMP t2)), dst = [], src = [t1, t2], jump = []}
      jtrue = IOPER {assem = BRANCH_ (ARM.B (same rop)) (L_ t), dst = [], src = [], jump = [t]}
  return $ i1 ++ i2 ++ [compare, jtrue] -- NO JFALSE AS FALSE BRANCH FOLLOWS THIS DIRECTLY

munchStm (IR.MOV e (CALL (NAME "#oneByte") [MEM me])) = do
   ret <- suffixStm (IR.MOV e (MEM me))
   return $ ret AL SB

munchStm (IR.MOV (CALL (NAME "#oneByte") [MEM me]) e) = do
   ret <- suffixStm (IR.MOV (MEM me) e)
   return $ ret AL B_

munchStm x = do
  m <- condStm x
  return $ m AL

-- ALLOW the suffix + cond of a load / store to change
suffixStm :: Stm -> State TranslateState (Cond -> SLType -> [ASSEM.Instr])
suffixStm (IR.MOV e (MEM me)) = do -- LDR
  (i, t) <- munchExp e
  (l, ts, op) <- munchMem me
  if null l then
    return (\c -> ( \suff -> i ++ [IMOV { assem = S_ (ARM.LDR suff c) (RTEMP t) op, src = ts, dst = [t]}]))
  else
    let s = head ts in
    return (\c -> (\suff -> i ++ l ++ [IMOV { assem = S_ (ARM.LDR suff c) (RTEMP t) (Imm (RTEMP s) 0), src = [s], dst = [t]}]))

suffixStm (IR.MOV (MEM me) e) = do -- STR
  (i, t) <- munchExp e
  (l, ts, op) <- munchMem me
  if null l then
    return (\c -> (\suff -> i ++ [IMOV { assem = S_ (ARM.STR suff c) (RTEMP t) op, src = ts, dst = [t]}]))
  else
    let s = head ts in
    return (\c -> (\suff -> i ++ l ++ [IMOV { assem = S_ (ARM.STR suff c) (RTEMP t) (Imm (RTEMP s) 0), src = [s], dst = [t]}]))

condStm :: Stm -> State TranslateState (Cond -> [ASSEM.Instr])  --allow for conditions to change
condStm ir@(IR.MOV e (MEM me)) = do
  ret <- suffixStm ir
  return (\c -> ret c W)

condStm ir@(IR.MOV (MEM me) e) = do
  ret <- suffixStm ir
  return (\c -> ret c W)

condStm (IR.PUSH e) = do
  (i, t) <- munchExp e
  return (\c -> i ++ [IMOV {assem = STACK_ (ARM.PUSH c) [RTEMP t], dst = [13], src = [t]}]) --sp here or not ??

condStm (IR.POP e) = do
  (i, t) <- munchExp e
  return (\c -> i ++ [IMOV {assem = STACK_ (ARM.POP c) [RTEMP t], dst = [t], src = [13]}])

condStm (IR.MOV e (CONSTI int)) = do
  (i, t) <- munchExp e
  return (\c -> i ++ [IMOV { assem = MC_ (ARM.MOV c) (RTEMP t) (IMM int), src = [], dst = [t]}])

condStm (IR.MOV e1 e2) = do  --In which sequence ?
  (i1, t1) <- munchExp e1
  (i2, t2) <- munchExp e2
  return (\c -> i1 ++ i2 ++ [move_to_r t2 t1])

condStm (JUMP e ls) = do
  (i, t) <- munchExp e
  return (\c -> [IOPER {assem = BRANCH_ (ARM.B c) (R_ (RTEMP t)), dst = [], src = [], jump = []}])

munchBuiltInFuncFrag :: Fragment -> State TranslateState [ASSEM.Instr]
munchBuiltInFuncFrag (PROC stm frame) = do
  munch <- munchStm stm
  return (pushlr : munch ++ [poppc])

munchDataFrag :: Fragment -> State TranslateState [ASSEM.Instr]
munchDataFrag (STRING label str)
  = return [ILABEL {assem = (M label (length str) str), lab = label}]

oneByte :: String -> Bool
oneByte "TBool" = True
oneByte "TChar" = True
oneByte _ = False

createPair :: String -> String -> [Exp] -> State TranslateState ([ASSEM.Instr], Temp)
-- pre : exps contains only two param
createPair s1 s2 exps = do
  (i1, t1) <- munchExp (exps !! 0)
  (i2, t2) <- munchExp (exps !! 1)
  tadddr <- newTemp -- pair addr
  let suffix1 = if (oneByte s1) then B_ else W
      suffix2 = if (oneByte s2) then B_ else W
      ld8 = IMOV { assem = (S_ (LDR W AL) R0 (NUM 8)), src = [], dst = [0]}
      ld4 = IMOV { assem = (S_ (LDR W AL) R0 (NUM 4)), src = [], dst = [0]}
      malloc = IOPER { assem = BRANCH_ (BL AL) (L_ "malloc"), src = [0], dst = [0], jump = ["malloc"]}
      strPairAddr = IMOV { assem = MC_ (ARM.MOV AL) (RTEMP tadddr) (R R0), src = [0], dst = [tadddr]}
      savefst = IMOV { assem = (S_ (STR suffix1 AL) (RTEMP t1) (Imm R0 0)), src = [t1, 0], dst = []}
      savesnd = IMOV { assem = (S_ (STR suffix2 AL) (RTEMP t2) (Imm R0 0)), src = [t2, 0], dst = []}
      strfstaddr = IMOV { assem = (S_ (STR W AL) R0 (Imm (RTEMP tadddr) 0)), src = [tadddr, 0], dst = []}
      strsndaddr = IMOV { assem = (S_ (STR W AL) R0 (Imm (RTEMP tadddr) 4)), src = [tadddr, 0], dst = []}
      strpaironstack= IMOV { assem = (S_ (STR W AL) (RTEMP tadddr) (Imm (RTEMP 13) 0)), src = [t1, 13], dst = [0]}
  return ([ld8, malloc, strPairAddr] ++ i1 ++ [ld4, malloc, savefst, strfstaddr]
           ++ i2 ++ [ld4, malloc, savesnd, strpaironstack], Frame.fp)

accessPair :: Bool -> String -> [Exp] -> State TranslateState ([ASSEM.Instr], Temp)
accessPair isfst typestr [e] = do
  (i, t) <- munchExp e
  let one = oneByte typestr
      offset = if isfst then 0 else 4
      getpaddr = move_to_r t 0
      check = IOPER { assem = BRANCH_ (BL AL) (L_ "p_check_null_pointer")
                      , src = [0], dst = [], jump = ["p_check_null_pointer"]}
      s1 = IMOV {assem = (S_ (LDR W AL) (RTEMP t) (Imm (RTEMP t) offset)), src = [t], dst = [t]}
      s2_suffix = if one then SB else W
      s2 = IMOV {assem = (S_ (LDR s2_suffix AL) (RTEMP t) (Imm (RTEMP t) 0)), src = [t], dst = [t]}
      s3_suffix = if one then B_ else W
      s3 = IMOV {assem = (S_ (STR s3_suffix AL) (RTEMP t) (Imm (RTEMP 13) 0)), src = [t, 13], dst = []}
  return (i ++ [getpaddr, check, s1, s2, s3], 13)

-------------------- Utilities ---------------------
condIR = [IR.EQ, IR.LT, IR.LE, IR.GT, IR.GE, IR.NE]
condARM = [ARM.EQ, ARM.LT, ARM.LE, ARM.GT, ARM.GE, ARM.NE]

invert :: ROp -> Cond
invert a = fromJust $ lookup a (zip condIR (reverse condARM))

same :: ROp -> Cond
same a = fromJust $ lookup a (zip condIR condARM)

deSeq :: Stm -> (Stm, Stm)
deSeq (SEQ s1 s2) = (s1, s2)

showStm stm = do
  putStrLn ""
  munch <- evalState (optimsedMunch stm) translateState
  putStrLn ""
  return $ ()

showExp exp = do
  munch <- evalState (munchExp exp) translateState
  return (munch)

-- munch file = do
--   ast <- parseFile file
--   ast' <- analyzeAST ast
--   let (ir, state) = runState (translateProgramF ast') translateState
--   m <- evalState (munchStm ir) state  -something wrong with ir
--   putStrLn $ show munch

translateState = TranslateState { levels = [],
                                  dataFrags = [],
                                  procFrags = [],
                                  tempAlloc = newTempAllocator,
                                  controlLabelAlloc = newLabelAllocator,
                                  dataLabelAlloc = newLabelAllocator,
                                  frameLabelAlloc = newLabelAllocator}

--- a sample ----
reg0 = TEMP 23
reg1 = TEMP 24
msg0 = "0"
msg1 = "1"
frame = Frame.newFrame "p_print_bool"
s1 = CJUMP IR.NE reg0 (CONSTI 0) "ne" "eq"
s_ne = SEQ (LABEL "ne") (IR.MOV reg0 (NAME msg0))
s_eq = SEQ (LABEL "eq") (IR.MOV reg0 (NAME msg1))
s2 = IR.MOV reg0 (BINEXP PLUS reg0 (CONSTI 4))
s3 = JUMP (NAME "printf") ["printf"]
s4 = IR.MOV reg0 (CONSTI 0)
s5 = JUMP (NAME "fflush") ["fflush"]
statement = SEQ (SEQ s1 (SEQ s_ne s_eq)) (SEQ s2 (SEQ s3 (SEQ s4 s5)))


s_1 = SEQ (IR.PUSH (TEMP 7)) (JUMP (NAME "something") ["something"])
s_2 = SEQ (LABEL "eq") (IR.MOV (TEMP 7) (CONSTI 5))
s_3 = CJUMP IR.GT (CONSTI 1) (CONSTI 2) "eq" "ne"
expr = ESEQ (SEQ s_3 (SEQ s_1 s_2)) (TEMP 7)

call = CALL (CONSTI 1) [(CONSTI 7)]
-- pre-Index sample --
assemPre = (IOPER { assem = (CBS_ (ARM.ADD NoSuffix ARM.EQ) (RTEMP 1) (RTEMP 1) (IMM 2)), src = [1], dst = [1], jump = []}) :
           (IMOV { assem = (S_ (ARM.LDR W ARM.EQ) (RTEMP 2) (Imm (RTEMP 1) 0)), src = [2], dst = [1]}) : []
irPre = IR.MOV (BINEXP MINUS (TEMP 2) (CONSTI 1)) (MEM (TEMP 2))

--load/store 1 byte sample
load1b = (IR.MOV (TEMP 2) (CALL (NAME "#oneByte") [MEM (CONSTI 1)]))
store1b = (IR.MOV (CALL (NAME "#oneByte") [MEM (CONSTI 1)]) (TEMP 2))
