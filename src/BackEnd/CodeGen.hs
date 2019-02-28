module BackEnd.CodeGen where

import FrontEnd.AST
import BackEnd.Translate
import BackEnd.Munch
import BackEnd.RegAlloc

codeGen :: ProgramF () -> IO String
codeGen ast = do
  let assemOut = munchInterface ast
  regAllocAssem assemOut
