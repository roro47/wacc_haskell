module BackEnd.Assem where

import BackEnd.Temp as Temp
import qualified BackEnd.Instructions as Arm
import BackEnd.IR 

data Instr = IOPER  { assem :: Arm.Instr,
                      dst :: [Temp.Temp],
                      src :: [Temp.Temp] }
           | ILABEL { assem :: Arm.Instr,
                      lab :: [Temp.Label] }
           | IMOV   { assem :: Arm.Instr,
                      dst  :: [Temp.Temp],
                      src :: [Temp.Temp] }
             -- for IMOV, length dst == 1, length src == 1
