module BackEnd.Munch where
import BackEnd.Instructions as ARM
import BackEnd.IR as IR
import BackEnd.Assem as ASSEM
import BackEnd.Temp
import Data.Maybe
import Control.Monad.State.Lazy
import BackEnd.Translate as T

type MunchExp = Maybe ([ASSEM.Instr], Temp)

munchExp :: Exp -> State TranslateState (MunchExp)
munchExp (TEMP t) = return $ Just ([],t)

munchExp (BINEXP PLUS e1 e2) = do
  Just (i1, t1) <- munchExp e1
  Just (i2, t2) <- munchExp e2
  let {this = IOPER {assem = CB_ (ARM.ADD NoSuffix AL) (RTEMP t1) (RTEMP t1) (R (RTEMP t2)),
              dst = [t1], src = [t1, t2], jump = []}}
  return $ Just (i1 ++ i2 ++ [this], t1)

munchExp (CONSTI i) = do
  t <- T.newTemp
  return $ Just ([IMOV {assem = MC_ (ARM.MOV AL) (RTEMP t) (IMM i) , dst = [t], src = []}], t)

munchExp (CONSTC c) = do
  t <- T.newTemp
  return $ Just ([IMOV {assem = MC_ (ARM.MOV AL) (RTEMP t) (CHR c) , dst = [t], src = []}], t)

munchExp (NAME l) = do
  t <- T.newTemp
  return $ Just ([IMOV {assem = S_ (ARM.LDR W AL) (RTEMP t) (MSG l) , dst = [t], src = []}], t)

munchExp _ = return Nothing
--
-- --TODO: munchexp remaining


munchStm :: Stm -> State TranslateState [ASSEM.Instr]
munchStm (IR.PUSH e) = do
  Just ([], t) <- munchExp e
  return [IMOV {assem = STACK_ (ARM.PUSH AL) [RTEMP t], dst = [t], src = []}]

munchStm (IR.POP e) = do
  Just ([], t) <- munchExp e
  return [IMOV {assem = STACK_ (ARM.POP AL) [RTEMP t], dst = [t], src = []}]

munchStm (LABEL label) = return [ILABEL {assem = BRANCH_ (BL AL) label, lab = [label]}]
munchStm (SEQ s1 s2) = do
  l1 <- munchStm s1
  l2 <- munchStm s2
  return $ l1 ++ l2
