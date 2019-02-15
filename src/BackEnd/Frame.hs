module BackEnd.Frame where

import BackEnd.Temp as Temp

data Frame = Frame String Int
            deriving (Eq, Show)

data Access = InFrame Int
            | InReg Temp.Temp
            deriving (Eq, Show)


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

-- allocate a new variable on the given frame
allocLocal :: Frame -> (Frame, Access)
allocLocal = undefined
