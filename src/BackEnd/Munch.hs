module BackEnd.Munch where
import BackEnd.Instructions as ARM
import BackEnd.IR as IR
import BackEnd.Assem as ASSEM
import BackEnd.Temp hiding (newTemp, newDataLabel)
import Data.Maybe
import Control.Monad.State.Lazy
import BackEnd.Translate as T
import BackEnd.Frame as Frame

type MunchExp = ([ASSEM.Instr], Temp)

optimsedMunch stm = do
  m <- munchStm stm
  return $ preIndexOptimise m

bopToCBS :: BOp ->  Maybe (Suffix -> Cond -> Calc)
bopToCBS bop
  = lookup bop [(IR.PLUS, ARM.ADD), (IR.MINUS, ARM.SUB),
            (IR.AND, ARM.AND), (IR.OR, ARM.ORR),
            (IR.LSHIFT, ARM.LSL), (IR.RSHIFT, ARM.LSR)]

munchExp :: Exp -> State TranslateState (MunchExp)
munchExp (BINEXP DIV e1 e2) = undefined  -- before  other BINEXP as it cannot be simplified
munchExp (BINEXP MOD e1 e2) = undefined  -- before  other BINEXP as it need special treatment

{-If munched stm is of length 2 here then it must be a SEQ conaing a naive stm and a label -}
munchExp (ESEQ (SEQ cj@(CJUMP rop _ _ _ _) (SEQ false true)) e) = do
  ci <- munchStm cj
  state <- get
  fi <- munchStm false
  if (length fi == 2) then
    do
    put state
    let (fc, fb) = deSeq false
    condf <- condStm fc
    brf <- munchStm fb
    state2 <- get
    ti <- munchStm true
    if(length ti == 2) then
      do
      put state2
      condt <- condStm (snd $ deSeq true) -- branch info not needed
      (i, t) <- munchExp e
      return ((condf (invert rop)) ++ (condt $ same rop) ++ i, t)
    else
      do
      (i, t) <- munchExp e
      return ((condf (invert rop)) ++ brf ++ ti ++ i, t)
  else
    do
    ti <- munchStm true
    (i, t) <- munchExp e
    return (ci ++ fi ++ ti ++ i, t)

munchExp (CALL f es) = do
  (fi, ft) <- munchExp f -- assume result returned in fi
  state <- get
  ls <- mapM (liftM fst.munchExp) es
  return (fi ++ concat ls, ft) --NO CALLER / CALLEE SAVE CONVENTION YET!

munchExp (TEMP t) = return ([],t)

munchExp x = do
  c <- condExp x
  return $ c AL

condExp :: Exp -> State TranslateState (Cond -> MunchExp)
condExp (BINEXP MUL e1 e2) = do
  (i1, t1) <- munchExp e1
  (i2, t2) <- munchExp e2
  tLo <- newTemp
  tHi <- newTemp
  return $ \c -> (i1 ++ i2 ++ [IMOV {assem = C2_ (SMULL NoSuffix c) (RTEMP tLo)
                 (RTEMP tHi) (RTEMP t1) (RTEMP t2),
                 src = [t1, t2], dst = [tLo, tHi]}], tLo)
  -- how to pass two of them ??? need special treatement

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
    otherwise -> return $ \c -> (i1 ++ [IOPER {assem = CBS_ ((fromJust cbs) NoSuffix c)
                                (RTEMP t1) (RTEMP t1) (R $ RTEMP t2),
                                dst = [t1], src = [t1, t2], jump = []}], t1)

condExp (CONSTI int) = do
  t <- newTemp
  return $ \c -> ([IMOV {assem = MC_ (ARM.MOV c) (RTEMP t) (IMM int) , dst = [t], src = []}], t)

condExp (CONSTC chr) = do
  t <- newTemp
  return $ \c -> ([IMOV {assem = MC_ (ARM.MOV c) (RTEMP t) (CHR chr) , dst = [t], src = []}], t)

condExp (NAME l) = do
  t <- newTemp
  return $ \c -> ([IMOV {assem = S_ (ARM.LDR W c) (RTEMP t) (MSG l) , dst = [t], src = []}], t)

munchMem :: Exp -> State TranslateState ([ASSEM.Instr], [Int], SLOP2)
--- PRE-INDEX ---
-- NOT HANDLED AT THIS STAGE; CAN USE PATTER MATCH TO OPTIMISE AFTERWARDS...
-- FOR EXAMPLE: [ADD R1 #4 , STR R4 R1] ==> [STR R4 [R1, #4]!]
-- HANDLED USING preIndexOptimise

-- TODO: more simplification allowed here : eval the e if possible to a int....
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
preIndexOptimise :: [ASSEM.Instr] -> [ASSEM.Instr]
preIndexOptimise (s1@(IOPER { assem = (CBS_ (ARM.ADD NoSuffix c) (RTEMP t11) (RTEMP t12) (IMM int))}) :
                  s2@(IMOV { assem = (S_ (ARM.LDR slt1 d) (RTEMP t21) (Imm (RTEMP t22) 0))}) :remain)
  | t11 == t12 && t22 == t11 && c == d
        = IMOV { assem = (S_ (ARM.LDR slt1 d) (RTEMP t21) (PRE (RTEMP t11) int)),src = [t11], dst = [t12]}
                : preIndexOptimise remain
  | otherwise = (s1 : s2 :preIndexOptimise remain)
preIndexOptimise (s1@(IOPER { assem = (CBS_ (ARM.ADD NoSuffix c) (RTEMP t11) (RTEMP t12) (IMM int))}) :
                  s2@(IMOV { assem = (S_ (ARM.STR slt1 d) (RTEMP t21) (Imm (RTEMP t22) 0))}) :remain)
  | t11 == t12 && t22 == t11 && c == d
        = IMOV { assem = (S_ (ARM.STR slt1 d) (RTEMP t21) (PRE (RTEMP t11) int)), src = [t11], dst = [t12]}
                  : preIndexOptimise remain
  | otherwise = (s1 : s2 :preIndexOptimise remain)
preIndexOptimise (x:xs) = x : (preIndexOptimise xs)
preIndexOptimise [] = []

munchStm :: Stm -> State TranslateState [ASSEM.Instr] -- everything with out condition

munchStm (LABEL label) = return [ILABEL {assem = LAB label, lab = [label]}]

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

munchStm x = do
  m <- condStm x
  return $ m AL

condStm :: Stm -> State TranslateState (Cond -> [ASSEM.Instr])  --allow for conditions to change

--- split save load to independent functions if possible to allow SLTYPE
  --- LDRSB ??? STRB ???? LACK OF ***INFOMATION***
condStm (IR.MOV e (MEM me)) = do -- LDR
  (i, t) <- munchExp e
  (l, ts, op) <- munchMem me
  if null l then
    return (\c -> i ++ [IMOV { assem = S_ (ARM.LDR W c) (RTEMP t) op, src = ts, dst = [t]}])
  else
    let s = head ts in
    return (\c -> i ++ l ++ [IMOV { assem = S_ (ARM.LDR W c) (RTEMP t) (Imm (RTEMP s) 0), src = [s], dst = [t]}])

condStm (IR.MOV (MEM me) e) = do -- STR
  (i, t) <- munchExp e
  (l, ts, op) <- munchMem me
  if null l then
    return (\c -> i ++ [IMOV { assem = S_ (ARM.STR W c) (RTEMP t) op, src = ts, dst = [t]}])
  else
    let s = head ts in
    return (\c -> i ++ l ++ [IMOV { assem = S_ (ARM.STR W c) (RTEMP t) (Imm (RTEMP s) 0), src = [s], dst = [t]}])

condStm (IR.PUSH e) = do
  (i, t) <- munchExp e
  return (\c -> i ++ [IMOV {assem = STACK_ (ARM.PUSH c) [RTEMP t], dst = [t], src = []}]) --sp here or not ??

condStm (IR.POP e) = do
  (i, t) <- munchExp e
  return (\c -> i ++ [IMOV {assem = STACK_ (ARM.POP c) [RTEMP t], dst = [t], src = []}])

condStm (IR.MOV e (CONSTI int)) = do
  (i, t) <- munchExp e
  return (\c -> i ++ [IMOV { assem = MC_ (ARM.MOV c) (RTEMP t) (IMM int), src = [], dst = [t]}])

condStm (IR.MOV e (CONSTC char)) = do
  (i, t) <- munchExp e
  return (\c -> i ++ [IMOV { assem = MC_ (ARM.MOV c) (RTEMP t) (CHR char), src = [], dst = [t]}])

condStm (IR.MOV e1 e2) = do
  (i1, t1) <- munchExp e1
  (i2, t2) <- munchExp e2
  return (\c -> i1 ++ i2 ++ [ IMOV { assem = MC_ (ARM.MOV c) (RTEMP t1) (R (RTEMP t2)),
                                     src = [t2], dst = [t1]}])

condStm (JUMP (NAME l) ls) = do
  return (\c -> [IOPER {assem = BRANCH_ (ARM.BL c) (L_ l), dst = [], src = [], jump = [l]}])

condStm (JUMP e ls) = do
  (i, t) <- munchExp e
  return (\c -> [IOPER {assem = BRANCH_ (ARM.B c) (R_ (RTEMP t)), dst = [], src = [], jump = []}])

munchBuiltInFuncFrag :: Fragment -> State TranslateState [ASSEM.Instr]
munchBuiltInFuncFrag (PROC stm frame) = do
  munch <- munchStm stm
  let push = IMOV {assem = STACK_ (ARM.PUSH AL) [LR], dst = [], src = [-2]}
      pop = IMOV {assem = STACK_ (ARM.POP AL) [PC], dst = [-1], src = []}
  return (push : munch ++ [pop])

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
  munch <- evalState (optimsedMunch stm) translateState
  return $ munch

showExp exp = do
  munch <- evalState (munchExp exp) translateState
  return munch

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

-- pre-Index sample --
assemPre = (IOPER { assem = (CBS_ (ARM.ADD NoSuffix ARM.EQ) (RTEMP 1) (RTEMP 1) (IMM 2)), src = [1], dst = [1], jump = []}) :
           (IMOV { assem = (S_ (ARM.LDR W ARM.EQ) (RTEMP 2) (Imm (RTEMP 1) 0)), src = [2], dst = [1]}) : []
irPre = IR.MOV (BINEXP PLUS (TEMP 2) (CONSTI 1)) (MEM (TEMP 2))
