module BackEnd.Frame where

import BackEnd.Temp as Temp
import BackEnd.IR 

data Frame = Frame { frameName :: String,
                     frameSize :: Int }
            deriving (Eq, Show)

data Access = InFrame Int
            | InReg Temp.Temp
            deriving (Eq, Show)

data Fragment = PROC Stm Frame
              | STRING Temp.Label String
              deriving (Eq, Show)

-- char is 1 byte
charSize :: Exp
charSize = CONST 1

-- int is 4 byte
intSize :: Exp
intSize = CONST 4

-- address
addrSize :: Exp
addrSize = CONST 4

-- frame pointer register
fp :: Temp.Temp
fp = 1

-- return value registemodule BackEnd.Translate where
--
rv :: Temp.Temp
rv = 2

-- return a new stack frame
newFrame :: String -> Frame
newFrame = undefined

-- Allocate a new variable on the given frame
-- Given true, then variable is passed by reference , hence is allocated
-- in stack.
-- Given false, then variable can be allocated in register
allocLocal :: Frame -> Type -> Bool -> Temp.TempAllocator ->
              (Frame, Access, Temp.TempAllocator)
allocLocal = undefined

externalCall :: String -> [Exp] -> Exp
externalCall sysFunc args = CALL (NAME sysFunc) args
