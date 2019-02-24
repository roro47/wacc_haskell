module BackEnd.Builtin where
import BackEnd.Instructions as ARM
import qualified BackEnd.Frame as Frame
import qualified BackEnd.Temp as Temp
import BackEnd.Assem as ASSEM
import FrontEnd.AST
import Control.Monad.State.Lazy
import BackEnd.Translate

{- This file contains all the built in fragmentations -}
{-  ##### Dependencies of the built in fragments #####
  println => p_print_ln
  print => p_print_int
           p_print_bool
           p_print_string
           p_print_reference
  read => p_read_int
          p_read_char

  subs => p_throw_overflow_error
  smull => p_throw_overflow_error
  div => p_check_divide_by_zero
  mod => p_check_divide_by_zero

  accessPairElem => p_check_null_pointer => p_throw_runtime_error
  accessArrayElem => p_check_array_bounds => p_throw_runtime_error
  p_free_pair => p_throw_runtime_error
  p_throw_overflow_error => p_throw_runtime_error
  p_check_divide_by_zero => p_throw_runtime_error

  p_throw_runtime_error => p_print_string

-}

p_print_ln :: State TranslateState [ASSEM.Instr]
p_print_ln = do
  msg <- newDataLabel
  addFragment (Frame.STRING msg "\0")
  return $[add_label "p_print_ln",
           pushlr,
           ld_msg_toR0 msg,
           r0_add4,
           ljump_to_label "puts",
           r0_clear,
           ljump_to_label "fflush",
           poppc]

p_print_int :: State TranslateState [ASSEM.Instr]
{-In ref compiler this temp is R1 -}
p_print_int = do
 msg <- newDataLabel
 temp <- newTemp
 addFragment (Frame.STRING msg "%d\0")
 return $[add_label "p_print_int",
          pushlr,
          move_to_r 0 temp,
          IMOV { assem = S_ (LDR W AL) (RTEMP temp) (MSG msg), src = [], dst = [temp]}]
          ++ end

p_print_bool :: State TranslateState [ASSEM.Instr]
p_print_bool = do
  truemsg <- newDataLabel
  falsemsg <- newDataLabel
  addFragment (Frame.STRING truemsg "true\0")
  addFragment (Frame.STRING falsemsg "false\0")
  return $[add_label "p_print_bool",
          pushlr,
          cmp_r0,
          ld_cond_msg_toR0 truemsg ARM.NE,
          ld_cond_msg_toR0 falsemsg ARM.EQ]
          ++ end


p_print_string :: State TranslateState [ASSEM.Instr]
p_print_string = do
  msg <- newDataLabel
  addFragment (Frame.STRING msg "%.*s\0")
  return $[add_label "p_print_string",
          pushlr,
          IMOV {assem = S_ (LDR W AL) R1 (Imm R0 0), src = [0], dst = [1]},
          IOPER { assem = CBS_ (ADD NoSuffix AL) R2 R0 (IMM 4), src = [0], dst = [2], jump = []},
          ld_msg_toR0 msg] ++ end

p_print_reference :: State TranslateState [ASSEM.Instr]
p_print_reference = do
  msg <- newDataLabel
  addFragment (Frame.STRING msg "%p\0")
  return $[add_label "p_print_reference",
          pushlr,
          move_to_r 0 1,
          ld_msg_toR0 msg] ++ end

p_check_null_pointer :: State TranslateState [ASSEM.Instr]
p_check_null_pointer = do
  msg <- newDataLabel
  addFragment (Frame.STRING msg "NullReferenceError: dereference a null reference\n\0")
  let s = "p_throw_runtime_error"
  return $[add_label "p_check_null_pointer",
          pushlr,
          cmp_r0,
          ld_cond_msg_toR0 msg ARM.EQ,
          ljump_cond s ARM.EQ,
          poppc]

p_throw_runtime_error :: State TranslateState [ASSEM.Instr]
p_throw_runtime_error = do
  let s = "p_print_string"
  return [add_label "p_throw_runtime_error",
          jump_to_label s,
          IMOV {assem = MC_ (ARM.MOV AL) R0 (IMM (-1)), src = [], dst = [0]},
          jump_to_label "exit"]

p_read_int :: State TranslateState [ASSEM.Instr]
p_read_int = do
  r <- p_read "%d\0"
  return $ (add_label "p_read_int"): r

p_read_char :: State TranslateState [ASSEM.Instr]
p_read_char = do
  r <- p_read " %c\0"
  return $ (add_label "p_read_char"): r

p_read str =  do
  msg <- newDataLabel
  addFragment (Frame.STRING msg str)
  return [pushlr,
          move_to_r 0 1,
          ld_msg_toR0 msg,
          r0_add4,
          ljump_to_label "scanf",
          poppc]

p_free_pair :: State TranslateState [ASSEM.Instr]
p_free_pair = do
  msg <- newDataLabel
  let str = "NullReferenceError: dereference a null reference\n\0"
      runTimeError = "p_throw_runtime_error"
  addFragment (Frame.STRING msg str)
  return [add_label "p_free_pair",
          pushlr,
          cmp_r0,
          ld_cond_msg_toR0 msg ARM.EQ,
          ljump_cond runTimeError ARM.EQ,
          IOPER { assem = STACK_ (PUSH AL) [R0], src = [0], dst = [Frame.fp], jump = []},
          IMOV {assem = S_ (LDR W AL) R0 (Imm R0 0), src = [0], dst = [0]},
          ljump_to_label "free",
          IMOV {assem = S_ (LDR W AL) R0 (Imm (RTEMP Frame.fp) 0), src = [Frame.fp], dst = [0]},
          IMOV {assem = S_ (LDR W AL) R0 (Imm R0 4), src = [0], dst = [0]},
          ljump_to_label "free",
          IOPER { assem = STACK_ (POP AL) [R0], src = [Frame.fp], dst = [Frame.fp, 0], jump = []},
          ljump_to_label "free",
          poppc]

{-How to handle array access in translate && munch? -}
p_check_array_bounds :: State TranslateState [ASSEM.Instr]
p_check_array_bounds = do
  msgneg <- newDataLabel
  msgover <- newDataLabel
  t <- newTemp -- r1
  addFragment (Frame.STRING msgneg "ArrayIndexOutOfBoundsError: negative index\n\0")
  addFragment (Frame.STRING msgover "ArrayIndexOutOfBoundsError: index too large\n\0")
  return [add_label "p_check_array_bounds",
          pushlr,
          cmp_r0,
          ld_cond_msg_toR0 msgneg ARM.LT,
          ljump_cond "p_throw_runtime_error" ARM.LT,
          IMOV {assem = S_ (LDR W AL) (RTEMP t) (Imm (RTEMP t) 0), src = [t], dst = [0]},
          cmp_r0,
          ld_cond_msg_toR0 msgover ARM.CS,
          ljump_cond "p_throw_runtime_error" ARM.CS,
          poppc]

{- where to call ? -}
p_throw_overflow_error :: State TranslateState [ASSEM.Instr]
p_throw_overflow_error = do
  msg <- newDataLabel
  addFragment (Frame.STRING msg $ "OverflowError: the result is too small/large"
                                   ++ " to store in a 4-byte signed-integer.\n")
  return [add_label "p_throw_overflow_error",
          ld_msg_toR0 msg, ljump_to_label "BL p_throw_runtime_error"]

{- where to call ? -}
p_check_divide_by_zero :: State TranslateState [ASSEM.Instr]
p_check_divide_by_zero = do
  msg <- newDataLabel
  addFragment (Frame.STRING msg "DivideByZeroError: divide or modulo by zero\n\0")
  return [add_label "p_check_divide_by_zero",
          pushlr,
          IOPER {assem = MC_ (CMP AL) R1 (IMM 0), src = [1], dst = [], jump = []},
          ld_cond_msg_toR0 msg ARM.EQ,
          ljump_cond "p_throw_runtime_error" ARM.EQ,
          poppc]

{- Short handed representations for assembly -}
move_to_r t r = IMOV {assem = MC_ (ARM.MOV AL) (RTEMP r) (R (RTEMP t)), src = [t], dst = [r]}
add_label = \s -> ILABEL { assem = LAB s, lab = s}

ljump_cond = \s -> (\c -> IOPER {assem = BRANCH_ (BL c) (L_ s),
                               src = [0], dst = [], jump = [s]})
--BL that takes r0 as param and no returns
ljump_to_label = \s -> (ljump_cond s AL)
--BL that takes no param and no returns
jump_to_label = \s -> IOPER {assem = BRANCH_ (BL AL) (L_ s),
                               src = [], dst = [], jump = [s]}
r0_add4 = IOPER { assem = CBS_ (ADD NoSuffix AL) R0 R0 (IMM 4), src = [0], dst = [0], jump = []}
r0_clear = IMOV {assem = MC_ (ARM.MOV AL) R0 (IMM 0), src = [], dst = [0]}
pushlr = IOPER { assem = STACK_ (PUSH AL) [LR], src = [Frame.ra], dst = [Frame.fp], jump = []}
poppc = IOPER { assem = STACK_ (POP AL) [PC], src = [Frame.pc, Frame.fp], dst = [Frame.fp], jump = []}
end = [r0_add4, ljump_to_label "printf", r0_clear, ljump_to_label "fflush", poppc]
ld_msg_toR0 = \msg -> IMOV {assem = S_ (LDR W AL) R0 (MSG msg), src = [], dst = [0]}
ld_cond_msg_toR0 = \msg -> (\c -> IMOV {assem = S_ (LDR W c) R0 (MSG msg), src = [], dst = [0]})
cmp_r0 = IOPER {assem = MC_ (CMP AL) R0 (IMM 0), src = [0], dst = [], jump = []}
