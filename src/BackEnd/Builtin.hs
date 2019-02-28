module BackEnd.Builtin where
import BackEnd.Instructions as ARM
import qualified BackEnd.Frame as Frame
import qualified BackEnd.Temp as Temp
import BackEnd.Assem as ASSEM
import FrontEnd.AST
import Control.Monad.State.Lazy

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
r0_clear = IMOV {assem = S_ (LDR W AL) R0 (NUM 0), src = [], dst = [0]}
pushlr = IOPER { assem = STACK_ (PUSH AL) [LR], src = [Frame.lr], dst = [13], jump = []}
poppc = IOPER { assem = STACK_ (POP AL) [PC], src = [Frame.pc, 13], dst = [13], jump = []}
end = [r0_add4, ljump_to_label "printf", r0_clear, ljump_to_label "fflush", poppc]
ld_msg_toR0 = \msg -> IMOV {assem = S_ (LDR W AL) R0 (MSG msg), src = [], dst = [0]}
ld_cond_msg_toR0 = \msg -> (\c -> IOPER {assem = S_ (LDR W c) R0 (MSG msg),
                                         src = [], dst = [0], jump = []})
cmp_r0 = IOPER {assem = MC_ (CMP AL) R0 (IMM 0), src = [0], dst = [], jump = []}
mv_r1_r0 = IMOV {assem = MC_ (ARM.MOV AL) R0 (R R1), src = [1], dst = [0]}
mv_r0_r1 = IMOV {assem = MC_ (ARM.MOV AL) R1 (R R0), src = [0], dst = [1]}
