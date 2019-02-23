module BackEnd.Canon where


import Control.Monad.State.Lazy
import Data.HashMap as HashMap hiding (map, filter)
import qualified BackEnd.Translate as Translate
import qualified BackEnd.Temp as Temp
import BackEnd.IR

data CanonState =
  CanonState { tempAlloc :: Temp.TempAllocator,
               controlLabelAlloc :: Temp.LabelAllocator}
    deriving (Eq, Show)

newTemp :: State CanonState Temp.Temp
newTemp = do
  state <- get
  let { (tempAlloc', temp) = Temp.newTemp (tempAlloc state) }
  put $ state { tempAlloc = tempAlloc' }
  return temp

newControlLabel :: State CanonState Temp.Label
newControlLabel = do
  state <- get
  let { (alloc, label) = Temp.newControlLabel (controlLabelAlloc state) }
  put $ state { controlLabelAlloc = alloc }
  return label


linearize :: Stm -> State CanonState [Stm]
linearize stm = do
  stm' <- doStm stm
  return $ linear stm' []
  where  linear :: Stm -> [Stm] -> [Stm]
         linear (SEQ a b) list = linear a (linear b list)
         linear s list = s:list

basicBlocks :: [Stm] -> State CanonState ([[Stm]], Temp.Label)
basicBlocks (l@(LABEL label):rest) = do
  let { (block, rest') = break (\e -> not $ isEND e) rest }
  (restBlock, nextLabel) <- basicBlocks rest'
  if length block == 0 || isLABEL (last block)
  then return (((l:block) ++ [JUMP (CONSTI 1) [nextLabel]]):restBlock
               , label)
  else return ((l:block):restBlock, label)
 where isJUMP (JUMP _ _) = True
       isJUMP _ = False
       isCJUMP (CJUMP _ _ _ _ _) = True
       isCJUMP _ = False
       isLABEL (LABEL _) = True
       isLABEL _ = False
       isEND e = (isJUMP e) || (isCJUMP e) || (isLABEL e)

basicBlocks stms@(stm:rest) = do
  label <- newControlLabel
  basicBlocks ((LABEL label):stms)
basicBlocks [] = do
  label <- newControlLabel
  return $ ([[LABEL label, JUMP (CONSTI 1) [label]]], label)


bLabel ((LABEL label):_) = label

traceSchedule :: [[Stm]] -> [Stm]
traceSchedule blocks = traceSchedule' blocks blockTable markTable
  where markTable = HashMap.empty
        blockTable = foldl addBlock HashMap.empty blocks
        addBlock acc b = insert (bLabel b) b acc

traceSchedule' [] _ _ = []
traceSchedule' (block:rest) blockTable markTable
 = trace ++ traceSchedule' rest' blockTable markTable'
 where (trace, markTable') =
         traceSchedule'' block blockTable markTable
       rest' =
         filter (\b -> not $ member (bLabel b) markTable') rest

traceSchedule'' block@((LABEL label):rest) blockTable markTable
  | unMarkedSucc /= [] = (block ++ trace, markTable'')
  | otherwise = (block, markTable')
  where nextBlock = blockTable ! (head unMarkedSucc)
        markTable' = insert label 1 markTable
        unMarkedSucc =
          filter (\l -> not $ member l markTable) (succs block)
        (trace, markTable'') =
          traceSchedule'' nextBlock blockTable markTable'
        succs block =
          case last block of
            JUMP _ labels -> labels
            CJUMP _ _ _ label1 label2 -> [label1, label2]


-- test whether two statements commute or not
commute :: Exp -> Stm -> Bool
commute (CONSTI _) _ = True
commute (CONSTC _) _ = True
commute _ NOP = False
commute _ _ = False

isESEQ :: Exp -> Bool
isESEQ (ESEQ _ _) = True
isESEQ _ = False

reorder :: [Exp] -> State CanonState (Stm, [Exp])
reorder (exp@(CALL _ _):rest) = do
  temp <- newTemp
  reorder ((ESEQ (MOV (TEMP temp) exp) (TEMP temp)):rest)
reorder (exp:rest) = do
  (stm', exps') <- doExp exp
  (stm2', exps2') <- reorder rest
  if commute exps' stm2'
  then return (SEQ stm' stm2', (exps':exps2'))
  else newTemp >>= \temp ->
       return (SEQ stm' (SEQ (MOV (TEMP temp) exps') stm2'),
               (TEMP temp):exps2')
reorder [] = return (NOP, [])

reorderStm :: [Exp] -> ([Exp] -> Stm) -> State CanonState Stm
reorderStm exps build = do
  (stm, exps') <- reorder exps
  return $ SEQ stm (build exps')

reorderExp :: [Exp] -> ([Exp] -> Exp) -> State CanonState (Stm, Exp)
reorderExp exps build = do
  (stm', exps') <- reorder exps
  return (stm', build exps')

doStm :: Stm -> State CanonState Stm
doStm (MOV (TEMP t) b)
  = reorderStm [b] (\(b:_) -> MOV (TEMP t) b)

doStm (MOV (MEM e) b)
  = reorderStm [e, b] (\(e:b:_) -> MOV (MEM e) b)

doStm (JUMP e labels)
  = reorderStm [e] (\(e:_) -> JUMP e labels)

doStm (CJUMP rop e1 e2 label1 label2)
  = reorderStm [e1, e2] (\(e1:e2:_) -> CJUMP rop e1 e2 label1 label2)

doStm (SEQ stm1 stm2) = do
  stm1' <- doStm stm1
  stm2' <- doStm stm2
  return $ SEQ stm1' stm2'

doStm stm = return stm

doExp :: Exp -> State CanonState (Stm, Exp)
doExp (BINEXP bop e1 e2)
  = reorderExp [e1, e2] (\(e1:e2:_) -> BINEXP bop e1 e2)

doExp (MEM e)
  = reorderExp [e] (\(e:_) -> MEM e)

doExp (CALL e es)
  = reorderExp (e:es) (\(e:es) -> CALL e es)

doExp (ESEQ stm e) = do
  stm' <- doStm stm
  (stm'', e') <- doExp e
  return (SEQ stm' stm'', e')

doExp e = reorderExp [] (\_ -> e)
