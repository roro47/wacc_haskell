module BackEnd.CodeGen where

import qualified Data.Set as Set
import Control.Monad.State.Lazy

import FrontEnd.AST
import qualified BackEnd.Translate as Translate
import BackEnd.Frame as Frame
import BackEnd.Canon as Canon
import BackEnd.Munch as Munch
import BackEnd.RegAlloc as RegAlloc
import BackEnd.Assem as Assem


codeGen :: ProgramF () -> IO String
codeGen ast = do
  let assemOut = evalState (instrGen ast) Translate.newTranslateState
  regAllocAssem assemOut

instrGen :: ProgramF () -> State Translate.TranslateState ([[Assem.Instr]], [[Assem.Instr]], [[Assem.Instr]])
instrGen ast = do
  stm <- Translate.translate ast
  stms <- Canon.transform stm
  dataFrags' <- dataFrags
  userFrags' <- liftM (map Munch.optimizeInstrs) userFrags
  code <- liftM Munch.optimizeInstrs (Munch.munchmany stms)
  builtInFrags' <- builtInFrags
  return (userFrags' ++ [code], dataFrags', builtInFrags')

dataFrags :: State Translate.TranslateState [[Assem.Instr]]
dataFrags = do
  state <-get
  return $ map Munch.munchDataFrag (Translate.dataFrags state)

userFrags :: State Translate.TranslateState [[Assem.Instr]]
userFrags = do
  state <- get
  let userFrags' = map (\(Frame.PROC stm _) -> stm) (Translate.procFrags state)
  mapM (\f -> transform f >>= \f' -> Munch.munchmany f') userFrags'

builtInFrags :: State Translate.TranslateState [[Assem.Instr]]
builtInFrags = do
  state <- get
  genProcFrags (Set.toList $ Translate.builtInSet state)

genProcFrags :: [Int] -> State Translate.TranslateState [[Assem.Instr]]
genProcFrags ids = do
  let gens = map (\n -> Munch.genBuiltIns !! n) ids
  pfrags <- foldM (\acc f -> f >>= \pfrag -> return $ acc ++ [pfrag]) [] gens
  return pfrags
