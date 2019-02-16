module BackEnd.Translate where

import Control.Monad.State.Lazy
import Data.HashMap as HashMap hiding (map)
import BackEnd.Frame as Frame
import BackEnd.IR


data Access = Access Frame.Frame Frame.Access

data EnvEntry = VarEntry Access Type
              | FunEntry Frame Temp.Label Type
              deriving (Eq, Show)

type Level = (Frame.Frame, Map String EnvEntry)

verifyLevels :: [Level] -> State [Level] [Level]
verifyLevels [] = fail "no frames available"
verifyLevels levels  = return levels

pushFrame :: String -> State [Level] ()
pushFrame name = put $ Frame.newFrame name

allocLocal :: State [Level] Access
allocLocal = do
  levels <- get
  ((frame, env):rest) <- verifyLevels levels
  let { (frame', access) = Frame.allocLocal frame }
  put ((frame', env):levels)
  return access

addVarEntry :: String -> Type -> State Frames ()
addVarEntry symbol t = do
  levels <


addFunEntry :: String -> Type -> State Frames ()
addFunEntry label t = do  
  levels <- get
  ((frame, env):rest) <- verifyLevels levels
  let { funEntry = FunEntry frame label t }
  put ((frame, insert label funEntry env):rest)
  return ()

translateProgramF :: ProgramF () -> Stm
translateProgramF (Ann (Program fs stats) _) = undefined

translateStat :: StatF () -> [Stm]
translateStat (Ann (Declare t (Ann (Ident id) _) expr) _) = undefined


