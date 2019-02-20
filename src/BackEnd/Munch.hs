module BackEnd.Munch where
import BackEnd.Instructions as ARM
import BackEnd.IR as IR
import BackEnd.Assem as ASSEM
import BackEnd.Temp
import Data.Maybe
import Control.Monad.State.Lazy
import BackEnd.Translate as T

type MunchExp = ([ASSEM.Instr], Temp)

bopToCBS :: BOp -> Suffix -> Cond ->  Maybe(Calc)
bopToCBS bop s cond
  = lookup bop [(IR.PLUS, ARM.ADD s cond), (IR.MINUS, ARM.SUB s cond),
            (IR.AND, ARM.AND s cond), (IR.OR, ARM.ORR s cond),
            (IR.LSHIFT, ARM.LSL s cond), (IR.RSHIFT, ARM.LSR s cond)]

munchExp :: Exp -> State TranslateState (MunchExp)
munchExp (BINEXP MUL e1 e2) = undefined  -- before  other BINEXP as it cannot be simplified
munchExp (BINEXP DIV e1 e2) = undefined  -- before  other BINEXP as it cannot be simplified
munchExp (BINEXP MOD e1 e2) = undefined  -- before  other BINEXP as it need special treatment

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

munchExp (ESEQ s e) = do
  -- munchStm s   TODO What the hell is happening here?
  return $ munchExp e

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


munchStm :: Stm -> State TranslateState [ASSEM.Instr]
munchStm (IR.PUSH e) = do
  ([], t) <- munchExp e
  return [IMOV {assem = STACK_ (ARM.PUSH AL) [RTEMP t], dst = [t], src = []}]

munchStm (IR.POP e) = do
  ([], t) <- munchExp e
  return [IMOV {assem = STACK_ (ARM.POP AL) [RTEMP t], dst = [t], src = []}]

munchStm (LABEL label) = return [ILABEL {assem = BRANCH_ (BL AL) label, lab = [label]}]
munchStm (SEQ s1 s2) = do
  l1 <- munchStm s1
  l2 <- munchStm s2
  return $ l1 ++ l2

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
