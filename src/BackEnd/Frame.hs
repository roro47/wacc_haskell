module BackEnd.Frame where

import FrontEnd.AST
import BackEnd.Temp as Temp
import BackEnd.IR

{- Note
   -----------------------------------------------------------------------------

   Frame contains provides a interface for specifying information related with
   executing assembly, such as stack, fragment, and so on.

   It intends to be an interface to capture specific detail (register, calling
   convention and so on), but we specified to ARM for convenience.

   The design is inspired by the book Modern Compiler Implementation in ML.
-}


-- call frame or frame for a scope
data Frame = Frame { frameName :: String,
                     frameSize :: Int }
            deriving (Eq, Show)

-- specify how to access a symbolic variable
data Access = InFrame Int        -- stored in specific offset from the base of the frame
            | InReg Temp.Temp    -- stored in temp
            deriving (Eq, Show)

-- fragments to be added as part of generated code
data Fragment = PROC Stm Frame                  -- procedure fragment (builtIn function
                                                -- or user-defined function)
              | STRING Temp.Label String Int    -- data fragment for string literal
              deriving (Eq, Show)

charSize :: Int
charSize = 4

boolSize :: Int
boolSize = 4

intSize :: Int
intSize = 4

addrSize :: Int
addrSize = 4


-- size of type to use variable of a type is stored on stack
-- all types are stored in 4 bytes on stack to keep stack pointer aligned
typeSize :: Type -> Int
typeSize t =
 case t of
   TChar -> 1
   TBool -> 1
   _ -> 4


-- ARM machine register
pc :: Temp.Temp
pc = 15

-- frame pointer register
fp :: Temp.Temp
fp = 11

sp :: Temp.Temp
sp = 13

-- link register, store return address
lr :: Temp.Temp
lr  = 14

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

-- system call or call to functions in dynamically linked libraries
externalCall :: String -> [Exp] -> Exp
externalCall sysFunc args = CALL (NAME sysFunc) args
