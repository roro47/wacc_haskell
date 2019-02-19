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

newDataLabel :: LabelAllocator -> (LabelAllocator, Label)
newDataLabel (LabelAllocator i) = (LabelAllocator $ i + 1, "msg_" ++ show i)

newControlLabel :: LabelAllocator -> (LabelAllocator, Label)
newControlLabel (LabelAllocator i) = (LabelAllocator $ i + 1, "label_" ++ show i)

newFrameLabel :: LabelAllocator -> (LabelAllocator, Label)
newFrameLabel (LabelAllocator i) = (LabelAllocator $ i + 1, "frame_" ++ show i)
