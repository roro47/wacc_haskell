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

bopToCBS :: BOp ->  Maybe (Suffix -> Cond -> Calc)
bopToCBS bop
  = lookup bop [(IR.PLUS, ARM.ADD), (IR.MINUS, ARM.SUB),
            (IR.AND, ARM.AND), (IR.OR, ARM.ORR),
            (IR.LSHIFT, ARM.LSL), (IR.RSHIFT, ARM.LSR)]

armCond :: ROp -> Cond
armCond c = fromJust $ lookup c [(IR.EQ, ARM.EQ), (IR.NE, ARM.NE), (IR.LT, ARM.LT), (IR.LE, ARM.LE),
                                 (IR.GT, ARM.GT), (IR.GE, ARM.GE)]

munchExp :: Exp -> State TranslateState (MunchExp)
munchExp (BINEXP DIV e1 e2) = undefined  -- before  other BINEXP as it cannot be simplified
munchExp (BINEXP MOD e1 e2) = undefined  -- before  other BINEXP as it need special treatment

{-If munched stm is of length 2 then it must be a SEQ conaing a naive stm and a label -}
munchExp (ESEQ (SEQ cj@(CJUMP rop _ _ _ _) (SEQ false true)) e) = do
  ci <- munchStm cj
  state <- get
  fi <- munchStm false
  if (length fi == 2) then
    do
    put state
    condf <- condStm (fst $ deSeq false)
    brf <- munchStm (snd $ deSeq false)
    state2 <- get
    ti <- munchStm true
    if(length ti == 2) then
      do
      put state2
      condt <- condStm (snd $ deSeq true)
      --no need the brach thing
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
      jtrue = IOPER {assem = BRANCH_ (ARM.B (armCond rop)) (L_ t), dst = [], src = [], jump = [t]}
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
  return (\c -> [IOPER {assem = BRANCH_ (ARM.B c) (L_ l), dst = [], src = [], jump = [l]}])

condStm (JUMP e ls) = do
  (i, t) <- munchExp e
  return (\c -> [IOPER {assem = BRANCH_ (ARM.B c) (R_ (RTEMP t)), dst = [], src = [], jump = []}])

-------------------- Utilities ---------------------

invert :: ROp -> Cond
invert IR.EQ = ARM.NE
invert IR.NE = ARM.EQ
invert IR.LT = ARM.GE
invert IR.GE = ARM.LT
invert IR.GT = ARM.LE
invert IR.LE = ARM.GT

same :: ROp -> Cond
same IR.EQ = ARM.EQ
same IR.NE = ARM.NE
same IR.LT = ARM.LT
same IR.GE = ARM.GE
same IR.GT = ARM.GT
same IR.LE = ARM.LE

deSeq :: Stm -> (Stm, Stm)
deSeq (SEQ s1 s2) = (s1, s2)

showStm stm = do
  munch <- evalState (munchStm stm) translateState
  return munch

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
s_3 = CJUMP IR.EQ (CONSTI 1) (CONSTI 2) "eq" "ne"
expr = ESEQ (SEQ s_3 (SEQ s_1 s_2)) (TEMP 7)
