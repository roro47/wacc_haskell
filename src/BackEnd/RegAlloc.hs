{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module BackEnd.RegAlloc where

import Control.Lens hiding (index)
import Control.Monad.State.Lazy
import qualified Data.List as List
import Data.HashMap as Hash hiding (map, (\\))
import qualified Data.Set as Set
import BackEnd.Frame as Frame
import BackEnd.MakeGraph
import qualified BackEnd.Assem as Assem
import BackEnd.Munch as Munch
import BackEnd.Temp as Temp
import qualified BackEnd.FlowGraph as FGraph
import qualified BackEnd.Liveness as Live

{- Note
   -----------------------------------------------------------------------------
   Register allocation using graph coloring.

   The design is inspired by the book Modern Compiler Implementation in ML.
   Please refer to the book for more specific detail of the algorithm.
-}


-- number of registers available
okColors :: [Temp.Temp]
okColors = [4, 5, 6, 7, 8, 9, 10, 11, 12]

precolouredReg = [0..15]

k :: Int
k = length okColors


data Instr = Instr { index :: Int,
                     instr :: Assem.Instr } deriving (Show)


-- all Instr encountered should have unique index
instance Eq Instr where
  i1 == i2 = index i1 == index i2

instance Ord Instr where
  i1 <= i2 = index i1 <= index i2


data RegAllocState =
  RegAllocState {
    _liveOut :: Hash.Map Int (Set.Set Temp.Temp),
    _program :: [Instr],
    _fgraph :: FGraph.FlowGraph,
    _precoloured :: [Temp.Temp],
    _initial :: [Temp.Temp],                -- all temp reg, not precolored and not processed
    _simplifyWorkList :: Set.Set Temp.Temp, -- list of low-degree non-move-related nodes
    _freezeWorkList :: Set.Set Temp.Temp,   -- low-degree move-related nodes
    _spillWorkList :: Set.Set Temp.Temp,    --  high degree nodes
    _spillNodes :: Set.Set Temp.Temp,       -- node marked for spilling during this round;
                                            -- intially empty
    _coalescedNodes :: Set.Set Temp.Temp,   -- registers that has been coalesced
    _coloredNodes :: Set.Set Temp.Temp,     -- nodes successfully colored
    _selectStack :: [Temp.Temp],            -- stack containing temporaries removed from the graph
     -- move sets
    _coalescedMoves :: Set.Set Instr,       -- moves that has been coalesced
    _constrainedMoves :: Set.Set Instr,     -- moves whose source and target interface
    _frozenMoves :: Set.Set Instr,          -- moves that will no longer be considered for
                                            -- coalesced
    _workListMoves :: Set.Set Instr,        -- moves enabled for possible coalescing
    _activeMoves :: Set.Set Instr,          -- moves not yet ready for coalescing
    _moveList :: Hash.Map Int (Set.Set Instr), -- a mapping from a node(tmep) to the list of
                                              --moves it is associated with
    -- other data structure
    _alias :: Hash.Map Temp.Temp Temp.Temp, -- a mapping of alias
    _adjSet :: Set.Set (Temp, Temp),        -- set of interferences edges in the graph
    _adjList :: Hash.Map Temp.Temp (Set.Set Temp.Temp), -- adjacency list representation of graph
    _degree :: Hash.Map Int Int,             -- degree of each node in interference graph
    _color :: Hash.Map Temp.Temp Int         -- map from temp to machine register
  }
makeLenses ''RegAllocState

newRegAllocState :: RegAllocState
newRegAllocState = RegAllocState {
  _liveOut = Hash.empty,
  _program = [],
  _fgraph = FGraph.newFlowGraph,
  _precoloured = [],
  _initial = [],
  _simplifyWorkList = Set.empty,
  _freezeWorkList = Set.empty,
  _spillWorkList = Set.empty,
  _spillNodes = Set.empty,
  _coalescedNodes = Set.empty,
  _coloredNodes = Set.empty,
  _selectStack = [],
  _coalescedMoves = Set.empty,
  _constrainedMoves = Set.empty,
  _frozenMoves = Set.empty,
  _workListMoves = Set.empty,
  _activeMoves = Set.empty,
  _moveList = Hash.empty,
  _alias = Hash.empty,
  _adjSet = Set.empty,
  _adjList = Hash.empty,
  _degree = Hash.empty,
  _color = Hash.fromList (zip precolouredReg precolouredReg)
}

isMoveInstruction :: Instr -> Bool
isMoveInstruction (Instr _ (Assem.IMOV _ _ _)) = True
isMoveInstruction _ = False


regAllocAssem :: ([[Assem.Instr]], [[Assem.Instr]], [[Assem.Instr]]) -> IO String
regAllocAssem (assems, dataFrags, builtInFrags) = do
  states <- mapM regAllocAssem' assems
  let colorMap = states
      colorMap' = map Hash.toList colorMap
      final = map (\(c, a) -> Assem.normAssem' c a) (zip colorMap' assems)
      totalOut = Assem.showAssem builtInFrags dataFrags (concat final)
  let assemOut = map (\s -> s ++ "\n") totalOut
  return (concat $ List.filter (\x -> not $ and $ map (=='\n') x) assemOut)

  where regAllocAssem' assem = do
          let flow = instrsToGraph assem
              (calcLiveOut, _) = Live.calcLiveness flow assem
              initial' = List.nub $ concat $ map Assem.assemReg assem
              precoloured' = precolouredReg
              regState = newRegAllocState {
                _liveOut = calcLiveOut,
                _program = map (\(id, inst) -> Instr id inst) (zip [0..] assem),
                _initial = initial' List.\\ precolouredReg,
                _precoloured = precoloured',
                _fgraph = flow }
              (a, regState') = runState (regAlloc >>= \_ -> getAlias 17) regState
          return $ _color regState'


regAlloc :: State RegAllocState ()
regAlloc = do
  build
  makeWorkList
  regAlloc'
  assignColors
  where regAlloc' :: State RegAllocState ()
        regAlloc' = do
          simplifyWorkList' <- use simplifyWorkList
          workListMoves' <- use workListMoves
          freezeWorkList' <- use freezeWorkList
          spillWorkList' <- use spillWorkList
          regAlloc'' simplifyWorkList' workListMoves' freezeWorkList' spillWorkList'

        regAlloc'' ::  Set.Set Temp.Temp ->  Set.Set Instr
                 -> Set.Set Temp.Temp ->  Set.Set Temp.Temp
                 -> State RegAllocState ()
        regAlloc'' simplifyWorkList workListMoves freezeWorkList spillWorkList
          | not (Set.null simplifyWorkList) = do { simplify; regAlloc' }
          | not (Set.null workListMoves) = do { coalesce; regAlloc' }
          | not (Set.null freezeWorkList) = do { freeze; regAlloc'}
          | not (Set.null spillWorkList) = do { selectSpill; regAlloc' }
          | otherwise = return ()

build :: State RegAllocState ()
build = do
  program' <- use program
  mapM_ build' program'
  where build' i@(Instr id inst) = do
          live <- uses liveOut (\liveOut -> liveOut ! id)
          def' <- uses fgraph (\fgraph -> (FGraph.def fgraph) ! id)
          use' <- uses fgraph (\fgraph -> (FGraph.use fgraph) ! id)
          if (isMoveInstruction i)
          then do
            mapM_  (addMoveNodes i) (Set.union (Set.fromList def') (Set.fromList use'))
            let Assem.IMOV _ (c:_) _ = inst
                live' = List.filter (/= c) (Set.toList live)
            mapM_ addEdge [(d, l) | d <- def', l <- live']
          else do
            let live' = Set.toList live
            mapM_ addEdge [(d, l) | d <- def', l <- live']
            if length def' == 2
            then addEdge (def' !! 0, def' !! 1)
            else return () 

        addMoveNodes :: Instr -> Temp.Temp -> State RegAllocState ()
        addMoveNodes inst n = do
          moveList %= Hash.insertWith (Set.union) n (Set.singleton inst)

addEdge :: (Temp.Temp, Temp.Temp) -> State RegAllocState ()
addEdge (u, v) = do
  adj <- use adjSet
  if not (Set.member (u, v) adj) && u /= v
  then do
    adjSet %= Set.insert (u, v)
    adjSet %= Set.insert (v, u)
    preCol <- use precoloured
    when (not (elem u preCol)) $ do
      adjList %= (\list -> Hash.insertWith (Set.union) u (Set.singleton v) list)
      degree %= (\degree' -> Hash.insertWith (+) u 1 degree')
    when (not (elem v preCol)) $ do
      adjList %= (\list -> Hash.insertWith (Set.union) v (Set.singleton u) list)
      degree %= (\degree' -> Hash.insertWith (+) v 1 degree')
  else return ()

makeWorkList :: State RegAllocState ()
makeWorkList = do
  initial' <- use initial
  degree' <- use degree
  let fs = map (\n -> f n (degree' ! n)) initial' -- partially apply f
  mapM_ (\(f, n) -> moveRelated n >>= \b -> f b) (zip fs initial') -- apply f
  initial .= []
  where f :: Temp.Temp -> Int -> Bool -> State RegAllocState ()
        f n d isMoveRelated
          | d >= k        = spillWorkList %= Set.insert n
          | isMoveRelated = freezeWorkList %= Set.insert n
          | otherwise     = simplifyWorkList %= Set.insert n

moveRelated :: Temp.Temp -> State RegAllocState Bool
moveRelated n = do
  nodeMoves' <- nodeMoves n
  return $ not $ Set.null nodeMoves'
  --return True

nodeMoves :: Temp.Temp -> State RegAllocState (Set.Set Instr)
nodeMoves n = do
  activeMoves' <- use activeMoves
  workListMoves' <- use workListMoves
  moveList' <- use moveList
  if not $ Hash.member n moveList'
  then return Set.empty
  else return $ Set.intersection (moveList' ! n) (Set.union activeMoves' workListMoves')


-- currently adjacent node in the graph
adjacent :: Temp.Temp -> State RegAllocState [Temp.Temp]
adjacent n = do
  adjNodes <- uses adjList (\adj -> adj ! n)
  --adjNodes <- uses adjList (\adj -> if Hash.member n adj then adj ! n else Set.empty)
  selectStack' <- uses selectStack Set.fromList
  coalscedNodes' <- use coalescedNodes
  return $ Set.toList $ Set.difference adjNodes (Set.union selectStack' coalscedNodes')

enableMoves :: [Temp.Temp] -> State RegAllocState ()
enableMoves nodes = do
  mapM_ f nodes
  where f :: Temp.Temp -> State RegAllocState ()
        f n = do
          moves <- liftM Set.toList (nodeMoves n)
          mapM_ g moves

        g :: Instr -> State RegAllocState ()
        g m = do
          activeMoves' <- use activeMoves
          when (Set.member m activeMoves') $ do
            activeMoves %= Set.delete m
            workListMoves %= Set.insert m

decrementDegree :: Temp.Temp -> State RegAllocState ()
decrementDegree m = do
  d <- uses degree (\degree -> if Hash.member m degree then degree ! m else 0)
  --d <- uses degree (\degree -> degree ! m)
  degree %= insert m (d - 1)
  when (d == k) $ do
    adj <- liftM Set.fromList (adjacent m)
    enableMoves (Set.toList (Set.insert m adj))
    spillWorkList %= Set.delete m
    isMoveRelated <- moveRelated m
    if isMoveRelated
    then freezeWorkList %= Set.insert m
    else simplifyWorkList %= Set.insert m

simplify :: State RegAllocState ()
simplify = do
  (n, simplifyWorkList') <- uses simplifyWorkList Set.deleteFindMin
  simplifyWorkList .= simplifyWorkList'
  selectStack %= (n:)
  adj <- adjacent n
  mapM_ decrementDegree adj

addWorkList :: Temp.Temp -> State RegAllocState ()
addWorkList u = do
  isPrecoloured <- uses precoloured (elem u)
  isMoveRelated <- moveRelated u
  d <- uses degree (\degree' -> degree' ! u)
  when (isPrecoloured && not isMoveRelated && d < k) (do
    freezeWorkList %= Set.delete u
    simplifyWorkList %= Set.insert u)

getAlias :: Temp.Temp -> State RegAllocState Temp.Temp
getAlias n = do
  isCoalesced <- uses coalescedNodes (Set.member n)
  a <- uses alias (\alias -> alias ! n)
  if isCoalesced
  then getAlias a
  else return n

ok :: Temp.Temp -> Temp.Temp -> State RegAllocState Bool
ok t r = do
  d <- uses degree (\degree' -> degree' ! t)
  isPrecoloured <- uses precoloured (elem t)
  isEdge <- uses adjSet (Set.member (t, r))
  return $ d < k && isPrecoloured && isEdge

conservative :: [Temp.Temp] -> State RegAllocState Bool
conservative nodes = do
  degree' <- use degree
  let { degrees = map (\n -> degree' ! n) nodes }
  let { k' = foldl (\acc d -> if d >= k then acc + 1 else acc) 0 degrees }
  return $ k' < k


combine :: (Temp.Temp, Temp.Temp) -> State RegAllocState ()
combine (u, v) = do
  isVFrozen <- uses freezeWorkList (Set.member v)
  if isVFrozen
  then do { freezeWorkList %= Set.delete v }
  else do { spillWorkList %= Set.delete v }
  coalescedNodes %= Set.insert v
  alias %= insert v u
  uMoveList <- uses moveList (\l -> l ! u)
  vMoveList <- uses moveList (\l -> l ! v)
  enableMoves [v]
  moveList %= insert u (Set.union uMoveList vMoveList)
  adj <- adjacent v
  mapM_ (\t -> do { addEdge (t, u); decrementDegree t }) adj
  d <- uses degree (\degree -> degree ! u)
  isUFrozen <- uses freezeWorkList (Set.member u)
  when (d >= k && isUFrozen) $ do
    freezeWorkList %= Set.delete u
    spillWorkList %= Set.insert u

freezeMoves :: Temp.Temp -> State RegAllocState ()
freezeMoves u = do
  nodeMoves' <- nodeMoves u
  mapM_ freezeMoves' (Set.toList nodeMoves')
  where freezeMoves' move@(Instr index mov) = do
          let { x = head $ Assem.src mov ; y = head $ Assem.dst mov }
          y' <- getAlias y
          u' <- getAlias u
          v <- do { if y' == u' then getAlias x else getAlias y }
          activeMoves %= Set.delete move
          frozenMoves %= Set.insert move
          nodeMoves' <- nodeMoves v
          d <- uses degree (\degree -> degree ! v)
          when (Set.null nodeMoves' && d < k) (do
            freezeWorkList %= Set.delete v
            simplifyWorkList %= Set.insert v)

freeze :: State RegAllocState ()
freeze = do
  (n, freezeWorkList') <- uses freezeWorkList Set.deleteFindMin
  freezeWorkList .= freezeWorkList'
  simplifyWorkList %= Set.insert n
  freezeMoves n


selectSpill :: State RegAllocState ()
selectSpill = do
  -- todo: use a heuristic to choose one that spills
  (m, freezeWorkList') <- uses spillWorkList Set.deleteFindMin
  freezeWorkList .= freezeWorkList'
  simplifyWorkList %= Set.insert m
  freezeMoves m

coalesce :: State RegAllocState ()
coalesce = do
  (m, workListMoves') <- uses workListMoves Set.deleteFindMin
  workListMoves .= workListMoves'
  let { x = head $ Assem.src $ instr m; y = head $ Assem.dst $ instr m }
  x' <- getAlias x
  y' <- getAlias y
  isPrecoloured <- uses precoloured (elem y')
  (u, v) <- do { if isPrecoloured then return (y', x') else return (x', y') }
  workListMoves %= Set.delete m
  -- prepare values for conditional operations in f
  adj <- use adjSet
  preCol <- use precoloured
  adjV <- adjacent v
  allOK <- mapM (\t-> ok t u) adjV
  adjU <- adjacent u
  adjV <- adjacent v
  conserv <- conservative (Set.toList $ Set.union (Set.fromList adjU) (Set.fromList adjV))
  f u v preCol adj (and allOK) conserv m

  where f :: Temp.Temp -> Temp.Temp -> [Temp.Temp] -> Set.Set (Temp.Temp, Temp.Temp)
             -> Bool -> Bool -> Instr -> State RegAllocState ()
        f u v preCol adj' allOK conserv m
          | u == v = do
              coalescedMoves %= Set.insert m
              addWorkList u
          | elem v preCol || Set.member (u, v) adj' = do
              constrainedMoves %= Set.insert m
              addWorkList u
              addWorkList v
          | (elem u preCol && allOK) || ((not (elem u preCol)) && conserv) = do
              coalescedMoves %= Set.insert m
              combine (u,v)
              addWorkList u
          | otherwise = do
              activeMoves %= Set.insert m

assignColors :: State RegAllocState ()
assignColors = do
  selectStack' <- use selectStack
  mapM assignColors' selectStack'
  --assignColors' (head selectStack')
  selectStack .= []
  coalescedNodes' <- use coalescedNodes
  mapM_ handleCoalesced coalescedNodes'
  where assignColors' :: Temp.Temp -> State RegAllocState ()
        assignColors' n = do
          ws <- uses adjList (\adjList -> adjList ! n)
          okColors' <- foldM filterColor okColors ws
          if okColors' == []
          then do
            spillNodes %= Set.insert n
          else do
            coloredNodes %= Set.insert n
            let { c = head okColors' }
            color %= Hash.insert n c

        filterColor :: [Temp.Temp] -> Temp.Temp -> State RegAllocState [Temp.Temp]
        filterColor okColors w = do
          w' <- getAlias w
          coloredNodes' <- use coloredNodes
          preCol <- use precoloured
          color' <- use color
          if Set.member w' (Set.union coloredNodes' (Set.fromList preCol))
          then return $ List.delete (color' ! w') okColors
          else return okColors

        handleCoalesced :: Temp.Temp -> State RegAllocState ()
        handleCoalesced n = do
          n' <- getAlias n
          color' <- use color
          color %= Hash.insert n (color' ! n')

rewriteProgram :: State RegAllocState ()
rewriteProgram = undefined
