module BackEnd.Assem where

import BackEnd.Temp as Temp
import qualified BackEnd.Instructions as Arm
import BackEnd.IR

data Instr = IOPER  { assem :: Arm.Instr,
                      dst :: [Temp.Temp],
                      src :: [Temp.Temp],
                      jump :: [Temp.Label]}
           | ILABEL { assem :: Arm.Instr,
                      lab :: [Temp.Label] }
           | IMOV   { assem :: Arm.Instr,
                      dst  :: [Temp.Temp],
                      src :: [Temp.Temp] }
           -- deriving Show
             -- for IMOV, length dst == 1, length src == 1

instance Prelude.Show Instr where
    show (IOPER assem  dst src jump) = Arm.output_show assem
    show (ILABEL assem lab) = Arm.output_show assem
    show (IMOV assem  dst src) = Arm.output_show assem
