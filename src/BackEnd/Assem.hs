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
    show (IOPER assem dst src jump) = Arm.output_show assem
    show (ILABEL assem lab) = Arm.output_show assem
    show (IMOV assem dst src) = Arm.output_show assem

normInstr :: (Arm.Instr -> Arm.Instr) -> Instr -> Instr
normInstr func (IOPER assem dst src jump) = IOPER (func assem) dst src jump
normInstr func (ILABEL assem lab) = ILABEL (func assem) lab
normInstr func (IMOV assem dst src) = IMOV (func assem) dst src

normAssem ::[(Int, Arm.REG)] -> [Instr] -> [Instr]
normAssem ((i, r):ls) instrs = normAssem ls (map (normInstr $ Arm.normInstr i r) instrs)
normAssem [] instrs = instrs
