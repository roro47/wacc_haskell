module BackEnd.Builtin where
import BackEnd.Instructions as ARM
import qualified BackEnd.Frame as Frame
import qualified BackEnd.Temp as Temp
import BackEnd.Assem as ASSEM
import FrontEnd.AST
import Control.Monad.State.Lazy
import BackEnd.Translate

{- This file contains all the built in fragmentations -}

p_print_int :: State TranslateState [ASSEM.Instr]
{-In ref compiler this temp is R1 -}
p_print_int = do
 msg <- newDataLabel
 temp <- newTemp
 addFragment (Frame.STRING msg "%d\0")
 return $[pushlr,
          move_to_r 0 temp,
          IMOV { assem = S_ (LDR W AL) (RTEMP temp) (MSG msg), src = [], dst = [temp]}]
          ++ end

p_print_bool :: State TranslateState [ASSEM.Instr]
p_print_bool = do
  truemsg <- newDataLabel
  falsemsg <- newDataLabel
  addFragment (Frame.STRING truemsg "true\0")
  addFragment (Frame.STRING falsemsg "false\0")
  return $[pushlr,
          cmp_r0,
          IMOV {assem = S_ (LDR W ARM.NE) R0 (MSG truemsg), src = [], dst = [0]},
          ldeq_msg_toR0 falsemsg]
          ++ end


p_print_string :: State TranslateState [ASSEM.Instr]
p_print_string = do
  msg <- newDataLabel
  addFragment (Frame.STRING msg "%.*s\0")
  return $[pushlr,
          IMOV {assem = S_ (LDR W AL) R1 (Imm R0 0), src = [0], dst = [1]},
          IOPER { assem = CBS_ (ADD NoSuffix AL) R2 R0 (IMM 4), src = [0], dst = [2], jump = []},
          ld_msg_toR0 msg] ++ end

p_print_reference :: State TranslateState [ASSEM.Instr]
p_print_reference = do
  msg <- newDataLabel
  addFragment (Frame.STRING msg "%p\0")
  return $[pushlr,
          move_to_r 0 1,
          ld_msg_toR0 msg] ++ end

p_check_null_pointer :: State TranslateState [ASSEM.Instr]
p_check_null_pointer = do
  msg <- newDataLabel
  addFragment (Frame.STRING msg "NullReferenceError: dereference a null reference\n\0")
  let s = "p_throw_runtime_error"
  return $[pushlr,
          cmp_r0,
          ldeq_msg_toR0 msg,
          IOPER {assem = BRANCH_ (BL ARM.EQ) (L_ s),
                 src = [0], dst = [], jump = [s]},
          poppc]

p_throw_runtime_error :: State TranslateState [ASSEM.Instr]
p_throw_runtime_error = do
  let s = "p_print_string"
  return [jumpt_to_label s,
          IMOV {assem = MC_ (ARM.MOV AL) R0 (IMM (-1)), src = [], dst = [0]},
          jumpt_to_label "exit"]

p_read_int :: State TranslateState [ASSEM.Instr]
p_read_int = p_read "%d\0"

p_read_char :: State TranslateState [ASSEM.Instr]
p_read_char = p_read " %c\0"

p_read str =  do
  msg <- newDataLabel
  addFragment (Frame.STRING msg str)
  return [pushlr,
          move_to_r 0 1,
          ld_msg_toR0 msg,
          r0_add4,
          ljumpt_to_label "scanf",
          poppc]

p_free_pair :: State TranslateState [ASSEM.Instr]
p_free_pair = do
  msg <- newDataLabel
  let str = "NullReferenceError: dereference a null reference\n\0"
      runTimeError = "p_throw_runtime_error"
  addFragment (Frame.STRING msg str)
  return [pushlr,
          cmp_r0,
          ldeq_msg_toR0 msg,
          IOPER {assem = BRANCH_ (BL ARM.EQ) (L_ runTimeError),
                 src = [0], dst = [], jump = [runTimeError]},
          IOPER { assem = STACK_ (PUSH AL) [R0], src = [0], dst = [Frame.fp], jump = []},
          IMOV {assem = S_ (LDR W AL) R0 (Imm R0 0), src = [0], dst = [0]},
          ljumpt_to_label "free",
          IMOV {assem = S_ (LDR W AL) R0 (Imm (RTEMP Frame.fp) 0), src = [Frame.fp], dst = [0]},
          IMOV {assem = S_ (LDR W AL) R0 (Imm R0 4), src = [0], dst = [0]},
          ljumpt_to_label "free",
          IOPER { assem = STACK_ (POP AL) [R0], src = [Frame.fp], dst = [Frame.fp, 0], jump = []},
          ljumpt_to_label "free",
          poppc
          ]



{- Short handed representations for assembly -}
move_to_r t r = IMOV {assem = MC_ (ARM.MOV AL) (RTEMP r) (R (RTEMP t)), src = [t], dst = [r]}
add_label = \s -> ILABEL { assem = LAB s, lab = [s]}
--BL that takes r0 as param and no returns
ljumpt_to_label = \s -> IOPER {assem = BRANCH_ (BL AL) (L_ s),
                               src = [0], dst = [], jump = [s]}
--BL that takes no param and no returns
jumpt_to_label = \s -> IOPER {assem = BRANCH_ (BL AL) (L_ s),
                               src = [], dst = [], jump = [s]}
r0_add4 = IOPER { assem = CBS_ (ADD NoSuffix AL) R0 R0 (IMM 4), src = [0], dst = [0], jump = []}
r0_clear = IMOV {assem = MC_ (ARM.MOV AL) R0 (IMM 0), src = [], dst = [0]}
pushlr = IOPER { assem = STACK_ (PUSH AL) [LR], src = [Frame.ra], dst = [Frame.fp], jump = []}
poppc = IOPER { assem = STACK_ (POP AL) [PC], src = [Frame.pc, Frame.fp], dst = [Frame.fp], jump = []}
end = [r0_add4, ljumpt_to_label "printf", r0_clear, ljumpt_to_label "fflush", poppc]
ld_msg_toR0 = \msg -> IMOV {assem = S_ (LDR W AL) R0 (MSG msg), src = [], dst = [0]}
ldeq_msg_toR0 = \msg -> IMOV {assem = S_ (LDR W ARM.EQ) R0 (MSG msg), src = [], dst = [0]}
cmp_r0 = IOPER {assem = MC_ (CMP AL) R0 (IMM 0), src = [0], dst = [], jump = []}
