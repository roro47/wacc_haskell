module BackEnd.Frame where

import FrontEnd.AST
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

charSize :: Int
charSize = 4

boolSize :: Int
boolSize = 4

intSize :: Int
intSize = 4

addrSize :: Int
addrSize = 4

typeSize :: Type -> Int
typeSize t =
 case t of
   TInt -> intSize
   TChar -> charSize
   TBool -> boolSize
   TStr -> addrSize
   TArray _ -> addrSize
   TPair _ _ -> addrSize
   _ -> undefined

pc :: Temp.Temp
pc = 15

-- frame pointer register
fp :: Temp.Temp
fp = 11

sp :: Temp.Temp
sp = 13

-- link register, store return address
ra :: Temp.Temp
ra  = 14

-- function return value register
rv :: Temp.Temp
rv = 0

-- function parameter register
param0 :: Temp.Temp
param0 = 0

param1 :: Temp.Temp
param1 = 1

param2 :: Temp.Temp
param2 = 2

param3 :: Temp.Temp
param3 = 3

dummy :: Temp.Temp
dummy = -1

-- return a new stack frame
newFrame :: String -> Frame
newFrame label = Frame label 0

-- Allocate a new variable on the given frame
-- Given false, then variable can be allocated in register
allocLocal :: Frame -> Type -> Bool -> Temp.TempAllocator ->
              (Frame, Access, Temp.TempAllocator)
allocLocal frame t escape tempAlloc =
  case escape of
    True -> (frame { frameSize = offset }, InFrame (-offset), tempAlloc )
    False -> (frame, InReg temp, tempAlloc')
  where (tempAlloc', temp) = newTemp tempAlloc
        offset = (frameSize frame) + allocSize
        allocSize = typeSize t
externalCall :: String -> [Exp] -> Exp
externalCall sysFunc args = CALL (NAME sysFunc) args
