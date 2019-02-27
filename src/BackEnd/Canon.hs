module BackEnd.Canon where

import Prelude hiding (EQ)
import Control.Monad.State
import Data.HashMap as HashMap hiding (map, filter)
import FrontEnd.Parser
import FrontEnd.SemanticAnalyzer
import qualified BackEnd.Translate as Translate
import qualified BackEnd.Temp as Temp
import BackEnd.IR

data CanonState =
  CanonState { tempAlloc :: Temp.TempAllocator,
               controlLabelAlloc :: Temp.LabelAllocator}
    deriving (Eq, Show)


testCanonFile :: String -> IO [Stm]
testCanonFile file = do
  ast <- parseFile file
  ast' <- analyzeAST ast
  let { (stm, s) = runState (Translate.translate ast') Translate.newTranslateState;
        canonState = CanonState { tempAlloc = Translate.tempAlloc s,
                                  controlLabelAlloc = Translate.controlLabelAlloc s};
        stms = evalState (transform stm) canonState }
  return stms


testBasicBlocksFile :: String -> IO [[Stm]]
testBasicBlocksFile file = do
  ast <- parseFile file
  ast' <- analyzeAST ast
  let { (stm, s) = runState (Translate.translate ast') Translate.newTranslateState;
        canonState = CanonState { tempAlloc = Translate.tempAlloc s,
                                  controlLabelAlloc = Translate.controlLabelAlloc s};
        stms = evalState (transform' stm) canonState }
  return stms


testLinearizeFile :: String -> IO [Stm]
testLinearizeFile file = do
  ast <- parseFile file
  ast' <- analyzeAST ast
  let { (stm, s) = runState (Translate.translate ast') Translate.newTranslateState;
        canonState = CanonState { tempAlloc = Translate.tempAlloc s,
                                  controlLabelAlloc = Translate.controlLabelAlloc s};
        stms = evalState (linearize stm) canonState }
  return stms


newCanonState :: CanonState
newCanonState = CanonState { tempAlloc = Temp.newTempAllocator,
                             controlLabelAlloc = Temp.newLabelAllocator }

testCanon :: Stm -> IO [Stm]
testCanon stm = do
  let { (trace, s) = runState (transform stm) newCanonState }
  return trace


testDoStm :: Stm -> IO Stm
testDoStm stm = do
  let { (stm', s) = runState (doStm stm) newCanonState }
  return stm'

testLinearize :: Stm -> IO [Stm]
testLinearize stm = do
  let { (stm', s) = runState (linearize stm) newCanonState }
  return $ filter (/= NOP) stm'

testBasicBlocks :: [Stm] -> IO [[Stm]]
testBasicBlocks stms = do
  let { (blocks, s) =
        runState (basicBlocks stms >>= \(bs,_) -> return bs) newCanonState }
  return blocks

testDoExp :: Exp -> IO Exp
testDoExp exp = do
  let { ((stm,exp'), s) = runState (doExp exp) newCanonState }
  return $ ESEQ stm exp'

transform :: Stm -> State CanonState [Stm]
transform stm = do
  stms <- linearize stm
  blocks <- basicBlocks stms
  return $ traceSchedule $ fst blocks

transform' :: Stm -> State CanonState [[Stm]]
transform' stm = do
  stms <- linearize stm
  (stms', _) <- basicBlocks stms
  return stms'

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
  return $ filter (/= NOP) $ linear stm' []
  where  linear :: Stm -> [Stm] -> [Stm]
         linear (SEQ a b) list = linear a (linear b list)
         linear s list = s:list

-- todo : need to take care of epilogue for done
basicBlocks :: [Stm] -> State CanonState ([[Stm]], Temp.Label)
basicBlocks [] = do
  label <- newControlLabel
  return $ ([[LABEL "done"]], "done")
basicBlocks (l@(LABEL label):rest) = do
  let { (block, rest') = break (\e -> isEND e) rest }
  if rest' == [] || isLABEL (head rest')
  then do
    (restBlock1, nextLabel1) <- basicBlocks rest'
    return (((l:block) ++ [JUMP (CONSTI 1) [nextLabel1]]):restBlock1, label)
  else do
    (restBlock2, _) <- basicBlocks $ tail rest'
    return (((l:block) ++ [head rest']):restBlock2, label)
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
          filter (\l -> not $ member l markTable') (succs block)
        (trace, markTable'') =
          traceSchedule'' nextBlock blockTable markTable'
        succs block =
          case last block of
            LABEL _ -> []
            JUMP _ labels -> labels
            CJUMP _ _ _ label1 label2 -> [label2, label1]


-- test whether two statements commute or not
commute :: Exp -> Stm -> Bool
commute (CONSTI _) _ = True
commute (CONSTC _) _ = True
commute _ NOP = True
commute _ _ = False

isESEQ :: Exp -> Bool
isESEQ (ESEQ _ _) = True
isESEQ _ = False

reorder :: [Exp] -> State CanonState (Stm, [Exp])
reorder (exp@(CALL _ _):rest) = do
  temp <- newTemp
  reorder $! ((ESEQ (MOV (TEMP temp) exp) (TEMP temp)):rest)
reorder (exp:rest) = do
  (stm', exps') <- doExp $! exp
  (stm2', exps2') <- reorder $! rest
  if commute exps' stm2'
  then return (SEQ stm' stm2', (exps':exps2'))
  else newTemp >>= \temp ->
       return (SEQ stm' (SEQ (MOV (TEMP temp) exps') stm2'),
               (TEMP temp):exps2')
reorder [] = return (NOP, [])

reorderStm :: [Exp] -> ([Exp] -> Stm) -> State CanonState Stm
reorderStm exps build = do
  (stm, exps') <- reorder $! exps
  return $ SEQ stm (build $! exps')

reorderExp :: [Exp] -> ([Exp] -> Exp) -> State CanonState (Stm, Exp)
reorderExp exps build = do
  (stm', exps') <- reorder exps
  return (stm', build $! exps')

doStm :: Stm -> State CanonState Stm

doStm (MOV (TEMP t) (CALL (NAME f) es))
  = reorderStm es (\es -> MOV (TEMP t) (CALL (NAME f) es))

doStm (MOV (MEM e) (CALL (NAME f) es))
  = reorderStm (e:es) (\(e:es) -> MOV (MEM e) (CALL (NAME f) es))

doStm (MOV (TEMP t) (CALL e es))
  = reorderStm (e:es) (\(e:es) -> MOV (TEMP t) (CALL e es))


doStm (MOV (TEMP t) b)
  = reorderStm [b] (\(b:_) -> MOV (TEMP t) b)

doStm stm@(MOV (MEM e@(BINEXP bop e1 e2)) b) = do
  if isOneLayer e1 && isOneLayer e2 && isOneLayer b
  then return stm
  else reorderStm [e, b] (\(e:b:_) -> MOV (MEM e) b)

doStm (MOV (MEM e) b)
  = reorderStm [e, b] (\(e:b_) -> MOV (MEM e) b)

doStm (MOV (ESEQ s e) b)
  = doStm (SEQ s (MOV e b))

doStm (PUSH e)
  = reorderStm [e] (\(e:_) -> PUSH e)

doStm (POP e)
  = reorderStm [e] (\(e:_) -> POP e)

doStm (JUMP e labels)
  = reorderStm [e] (\(e:_) -> JUMP e labels)

doStm (CJUMP rop e1 e2 label1 label2)
  = reorderStm [e1, e2] (\(e1:e2:_) -> CJUMP rop e1 e2 label1 label2)

doStm (SEQ stm1 stm2) = do
  stm1' <- doStm stm1
  stm2' <- doStm stm2
  return $ SEQ stm1' stm2'

doStm (EXP (CALL (NAME f) es))
  = reorderStm es (\es -> EXP (CALL (NAME f) es))

doStm (EXP (CALL e es))
  = reorderStm (e:es) (\(e:es) -> EXP (CALL e es))
doStm (EXP e)
  = reorderStm [e] (\(e:_) -> EXP e)

doStm stm = return stm

isOneLayer :: Exp -> Bool
isOneLayer (CONSTI _) = True
isOneLayer (CONSTC _) = True
isOneLayer (TEMP _) = True
isOneLayer (MEM _) = True
isOneLayer (NAME _) = True
isOneLayer e = False


doExp :: Exp -> State CanonState (Stm, Exp)
doExp exp@(MEM e@(BINEXP bop e1 e2)) = do
  if isOneLayer e1 && isOneLayer e2
  then return (NOP, exp)
  else reorderExp [e] (\(e:_) -> MEM e)

doExp exp@(BINEXP bop e1 e2) = do
  if isOneLayer e1 && isOneLayer e2
  then return (NOP, exp)
  else reorderExp [e1, e2] (\(e1:e2:_) -> BINEXP bop e1 e2)

doExp (MEM e)
  = reorderExp [e] (\(e:_) -> MEM e)

doExp (CALL (NAME f) es)
  = reorderExp es (\es -> CALL (NAME f) es)

doExp (CALL e es)
  = reorderExp (e:es) (\(e:es) -> CALL e es)

doExp (ESEQ stm e) = do
  stm' <- doStm stm
  (stm'', e') <- doExp e
  return (SEQ stm' stm'', e')

doExp e = reorderExp [] (\_ -> e)


t0 = TEMP 0
t1 = TEMP 1
t2 = TEMP 2

e1 = CONSTI 1

s1 = MOV t1 t2
s2 = MOV t2 t0


label1 = "label1"
label2 = "label2"
label3 = "label3"

testESEQ1 = ESEQ NOP (ESEQ NOP e1)
testESEQ2 = ESEQ NOP (ESEQ s1 e1)
testESEQ3 = BINEXP PLUS (ESEQ s1 (CONSTI 3)) (CONSTI 5)
testESEQ4 = BINEXP PLUS (CONSTI 1) (ESEQ (MOV (TEMP 23) (TEMP 90)) (CONSTI 79))
testESEQ5 = BINEXP PLUS (MEM (CONSTI 23)) (ESEQ (MOV (TEMP 0) (TEMP 1)) (CONSTI 29))


testLinear1 = SEQ (MOV t0 t1) (MOV t0 t2)
testLinear2 = SEQ (MOV t0 t1) (MOV t0 (ESEQ (MOV t0 t1) (CONSTI 1)))
testLinear3 = SEQ (MOV t0 t1) (MOV t0 (ESEQ (MOV t2 (CONSTI 1)) t2))
testLinear4 = SEQ (MOV t0 t1) (MOV t0 (BINEXP PLUS (ESEQ s1 (CONSTI 3)) (CONSTI 5)))
testLinear5 = SEQ (MOV t0 t1) (MOV t0 testESEQ4)
testLinear6 = SEQ (MOV t0 t1) (MOV t0 testESEQ5)
testLinear7 = MOV t0 (CALL (NAME "function") [])
testLinear8 = MOV t1 (CALL (NAME "function") [CONSTI 1, CONSTI 45])
testLinear9 = MOV t0 (CALL (NAME "function") [MEM (TEMP 13)])

testBasicBlocks1 = []
testBasicBlocks2 = [LABEL label1,
                    MOV t0 t1,
                    JUMP (CONSTI 1) [label1]]
testBasicBlocks3 = [LABEL label1,
                    MOV t0 t1,
                    LABEL label2,
                    MOV t2 t0,
                    JUMP (CONSTI 1) [label2]]
testBasicBlocks4 = [LABEL label1,
                    MOV t0 t1,
                    CJUMP EQ (CONSTI 1) (CONSTI 2) label1 label2,
                    MOV t2 t0]

testTraceSchedule1 = [[LABEL label1,
                       NOP,
                       JUMP (CONSTI 1) [label2]],
                       [LABEL label2,
                        JUMP (CONSTI 1) [label2]]]
