module BackEnd.Temp where

type Temp = Int
type Label = String

data TempAllocator = TempAllocator Int deriving (Eq, Show)
data LabelAllocator = LabelAllocator Int deriving (Eq, Show)


newTempAllocator :: TempAllocator
newTempAllocator = TempAllocator 0

-- return a new temporary, and a new allocator
newTemp :: TempAllocator -> (TempAllocator, Temp)
newTemp (TempAllocator i) = (TempAllocator $ i + 1, i)

newLabelAllocator :: LabelAllocator
newLabelAllocator = LabelAllocator 0

newLabel :: LabelAllocator -> (LabelAllocator, Label)
newLabel (LabelAllocator i) = (LabelAllocator $ i + 1, "msg_" ++ show i) 
