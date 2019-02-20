module BackEnd.Munch where
import BackEnd.Instructions as ARM
import BackEnd.IR as IR
import BackEnd.Assem as ASSEM
import BackEnd.Temp hiding (newTemp)
import Data.Maybe
import Control.Monad.State.Lazy
import BackEnd.Translate as T

type MunchExp = ([ASSEM.Instr], Temp)

bopToCBS :: BOp -> Suffix -> Cond ->  Maybe Calc
bopToCBS bop s cond
  = lookup bop [(IR.PLUS, ARM.ADD s cond), (IR.MINUS, ARM.SUB s cond),
            (IR.AND, ARM.AND s cond), (IR.OR, ARM.ORR s cond),
            (IR.LSHIFT, ARM.LSL s cond), (IR.RSHIFT, ARM.LSR s cond)]

armCond :: ROp -> Cond
armCond c = fromJust $ lookup c [(IR.EQ, ARM.EQ), (IR.NE, ARM.NE), (IR.LT, ARM.LT), (IR.LE, ARM.LE),
                                 (IR.GT, ARM.GT), (IR.GE, ARM.GE)]

munchExp :: Exp -> State TranslateState (MunchExp)
munchExp (BINEXP DIV e1 e2) = undefined  -- before  other BINEXP as it cannot be simplified
munchExp (BINEXP MOD e1 e2) = undefined  -- before  other BINEXP as it need special treatment

munchExp (BINEXP MUL e1 e2) = do
  (i1, t1) <- munchExp e1
  (i2, t2) <- munchExp e2
  tLo <- newTemp
  tHi <- newTemp
  let mul = IOPER {assem = C2_ (SMULL NoSuffix AL) (RTEMP tLo) (RTEMP tHi) (RTEMP t1) (RTEMP t2),
                   src = [t1, t2], dst = [tLo, tHi], jump = []}
  return $ (i1 ++ i2 ++ [mul], tLo) -- how to pass two of them ???

munchExp (BINEXP bop e (CONSTI int)) = do
  (i1, t1) <- munchExp e
  let {cbs = bopToCBS bop NoSuffix AL}
  if cbs /= Nothing then
    let {this = IOPER {assem = CBS_ (fromJust cbs) (RTEMP t1) (RTEMP t1) (IMM int),
         dst = [t1], src = [t1], jump = []}} in return (i1 ++ [this], t1)
  else
      fail ""
-- CONSTC???
munchExp (BINEXP bop e1 e2) = do
  (i1, t1) <- munchExp e1
  (i2, t2) <- munchExp e2
  let {cbs = bopToCBS bop NoSuffix AL}
  if cbs /= Nothing then
    let {this = IOPER {assem = CBS_ (fromJust cbs) (RTEMP t1) (RTEMP t1) (R $ RTEMP t2),
         dst = [t1], src = [t1, t2], jump = []}} in return (i1 ++ [this], t1)
  else
      fail ""

munchExp (CALL f es) = do
  (fi, ft) <- munchExp f -- assume result returned in fi
  state <- get
  ls <- mapM (liftM fst.munchExp) es
  return (fi ++ concat ls, ft)

munchExp (ESEQ s e) = do  --not sure
  ls <- munchStm s
  (i, t) <- munchExp e
  return (ls ++ i, t)

munchExp (CONSTI i) = do
  t <- T.newTemp
  return ([IMOV {assem = MC_ (ARM.MOV AL) (RTEMP t) (IMM i) , dst = [t], src = []}], t)

munchExp (CONSTC c) = do
  t <- T.newTemp
  return ([IMOV {assem = MC_ (ARM.MOV AL) (RTEMP t) (CHR c) , dst = [t], src = []}], t)

munchExp (TEMP t) = return ([],t)

munchExp (NAME l) = do
  t <- T.newTemp
  return ([IMOV {assem = S_ (ARM.LDR W AL) (RTEMP t) (MSG l) , dst = [t], src = []}], t)

munchExp _ = fail ""
--
-- --TODO: munchexp remaining


munchStm :: Stm -> State TranslateState [ASSEM.Instr] -- everything with out condition

munchStm (LABEL label) = return [ILABEL {assem = LAB label, lab = [label]}]

munchStm (SEQ s1 s2) = do
  l1 <- munchStm s1
  l2 <- munchStm s2
  return $ l1 ++ l2

munchStm (CJUMP rop e1 e2 t f) = do
  (i1, t1) <- munchExp e1
  (i2, t2) <- munchExp e2 --false branch followed first
  let compare = IOPER {assem = MC_ (ARM.CMP AL) (RTEMP t1) (R (RTEMP t2)), dst = [], src = [t1, t2], jump = []}
      jtrue = IOPER {assem = BRANCH_ (ARM.B (armCond rop)) (L_ t), dst = [], src = [], jump = [t]}
      jfalse = IOPER {assem = BRANCH_ (ARM.B AL) (L_ f), dst = [], src = [], jump = [f]}
  return $ i1 ++ i2 ++ [compare, jtrue, jfalse]

munchStm x = do 
  m <- munchStm_ x
  return $ m AL

munchStm_ :: Stm -> State TranslateState (Cond -> [ASSEM.Instr])  --allow for conditions to change
munchStm_ (IR.PUSH e) = do
  (i, t) <- munchExp e
  return (\c -> i ++ [IMOV {assem = STACK_ (ARM.PUSH c) [RTEMP t], dst = [t], src = []}]) --sp here or not ??

munchStm_ (IR.POP e) = do
  (i, t) <- munchExp e
  return (\c -> i ++ [IMOV {assem = STACK_ (ARM.POP c) [RTEMP t], dst = [t], src = []}])

munchStm_ (IR.MOV e (CONSTI int)) = do
  (i, t) <- munchExp e
  return (\c -> i ++ [IMOV { assem = MC_ (ARM.MOV c) (RTEMP t) (IMM int), src = [], dst = [t]}])
  
munchStm_ (IR.MOV e (CONSTC char)) = do
  (i, t) <- munchExp e
  return (\c -> i ++ [IMOV { assem = MC_ (ARM.MOV c) (RTEMP t) (CHR char), src = [], dst = [t]}])
  
munchStm_ (IR.MOV e1 e2) = do
  (i1, t1) <- munchExp e1
  (i2, t2) <- munchExp e2
  return (\c -> i1 ++ i2 ++ [ IMOV { assem = MC_ (ARM.MOV c) (RTEMP t1) (R (RTEMP t2)),
                                     src = [t2], dst = [t1]}])

munchStm_ (JUMP (NAME l) ls) = do --assume all E are lables !!
  return (\c -> [IOPER {assem = BRANCH_ (ARM.B c) (L_ l), dst = [], src = [], jump = [l]}])

munchStm_ (JUMP e ls) = do 
  (i, t) <- munchExp e
  return (\c -> [IOPER {assem = BRANCH_ (ARM.B c) (R_ (RTEMP t)), dst = [], src = [], jump = []}])

-------------------- Utilities ---------------------

showStm stm = do
  munch <- evalState (munchStm stm) translateState
  return $ munch

showExp exp = do
  munch <- evalState (munchExp exp) translateState
  return $ munch

translateState = TranslateState { levels = [],
                                  dataFrags = [],
                                  procFrags = [],
                                  tempAlloc = newTempAllocator,
                                  controlLabelAlloc = newLabelAllocator,
                                  dataLabelAlloc = newLabelAllocator,
                                  frameLabelAlloc = newLabelAllocator}
