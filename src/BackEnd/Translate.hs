module BackEnd.Translate where

import Control.Monad.State.Lazy
import Data.HashMap as HashMap hiding (map)
import BackEnd.Frame as Frame
import BackEnd.IR

data Access = Access Frame.Frame Frame.Access

data EnvEntry = VarEntry Access Type
              | FunEntry Frame Type
              deriving (Eq, Show)

type Level = (Frame.Frame, Map String EnvEntry)

translateProgramF :: ProgramF () -> Stm
translateProgramF (Ann (Program fs stats) _) = undefined

translateStat :: StatF () -> [Stm]
translateStat (Ann (Declare t (Ann (Ident id) _) expr) _) = undefined


pushFrame :: String -> State Frames ()
pushFrame name = put $ Frame.newFrame name

allocLocal :: State Frames Access
allocLocal = do
  (frames, env) <- get
  case frames of
    [] -> fail "no frames available "
    (frame:rest) -> do
      let { (frame', access) = Frame.allocLocal frame }
      put   
      
  
