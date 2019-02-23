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
 return [ move_to_r 0 temp,
          IMOV { assem = S_ (LDR W AL) (RTEMP temp) (MSG msg), src = [], dst = [temp]},
          r0_add4,
          ljumpt_to_label "print",
          r0_clear,
          ljumpt_to_label "fflush",
          poppc]


move_to_r t r = IMOV {assem = MC_ (ARM.MOV AL) (RTEMP r) (R (RTEMP t)), src = [t], dst = [r]}
add_label = \s -> ILABEL { assem = LAB s, lab = [s]}
--BL that takes r0 as param and no returns
ljumpt_to_label = \s -> IOPER {assem = BRANCH_ (BL AL) (L_ s),
                               src = [0], dst = [], jump = [s]}
r0_add4 = IOPER { assem = CBS_ (ADD NoSuffix AL) R0 R0 (IMM 4), src = [0], dst = [0], jump = []}
r0_clear = IMOV {assem = MC_ (ARM.MOV AL) R0 (IMM 0), src = [], dst = [0]}
pushlr = IOPER { assem = STACK_ (PUSH AL) [LR], src = [Frame.ra], dst = [Frame.fp], jump = []}
poppc = IOPER { assem = STACK_ (POP AL) [PC], src = [Frame.pc], dst = [Frame.fp], jump = []}
