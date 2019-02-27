module BackEnd.Liveness where

import Prelude hiding (succ)
import Data.HashMap as HashMap hiding (map)
import Data.Set as Set hiding (map, foldl)
import Algebra.Graph.AdjacencyMap as AdjMap
import BackEnd.MakeGraph
import BackEnd.Instructions as ARM
import BackEnd.Assem as Assem
import BackEnd.Temp as Temp
import qualified BackEnd.FlowGraph as FGraph

-- interference graph
data IGraph = IGraph { graph :: AdjMap.AdjacencyMap Int }
              deriving (Show)


-- Utility
use :: Int -> FGraph.FlowGraph -> Set.Set Int
use n fGraph = Set.fromList (FGraph.use fGraph ! n) 

def :: Int -> FGraph.FlowGraph -> Set.Set Int
def n fGraph = Set.fromList (FGraph.def fGraph ! n)

succ :: Int -> FGraph.FlowGraph -> [Int] 
succ n fGraph = Set.toList (postSet n (FGraph.control fGraph))

type LiveMap = HashMap.Map Int (Set.Set Temp.Temp)


testLiveness :: [Assem.Instr] -> (Int, (LiveMap, LiveMap))
testLiveness instrs = calcLiveness fgraph instrs
  where fgraph = instrsToGraph instrs

-- given flow graph, calculate live map
-- live map: given a node, return the set of node that are live at that point
-- return (liveIn, liveOut)
calcLiveness :: FGraph.FlowGraph -> [Assem.Instr] -> (Int, (LiveMap, LiveMap))
calcLiveness fgraph instrs = calcLiveness' nodes fgraph 0 liveIn liveOut
  where nodes = reverse [1..length instrs]
        -- assign nodes to empty set
        liveIn =  foldl (\acc x -> HashMap.insert x Set.empty acc) HashMap.empty nodes
        liveOut = liveIn

-- update liveIn and liveOut iteratively, stop until liveIn and liveOut stop changing
calcLiveness' :: [Int] -> FGraph.FlowGraph -> Int ->  LiveMap -> LiveMap -> (Int, (LiveMap, LiveMap))
calcLiveness' nodes fgraph n liveIn liveOut
  | (liveIn == liveIn' && liveOut == liveOut') = (n, (liveIn, liveOut))
  | otherwise = calcLiveness' nodes fgraph (n+1) liveIn' liveOut'
  where (liveIn', liveOut') = calcLiveness'' nodes fgraph liveIn liveOut 

-- Given a list of node, update liveIn and liveOut by examine use and def of each node
calcLiveness'' :: [Int] -> FGraph.FlowGraph -> LiveMap -> LiveMap -> (LiveMap, LiveMap)
calcLiveness'' [] _ liveIn liveOut = (liveIn, liveOut)
calcLiveness'' (n:ns) fGraph liveIn liveOut
  = calcLiveness'' ns fGraph liveIn' liveOut'
  where use' = use n fGraph
        def' = def n fGraph
        succ' = succ n fGraph
        liveIn' = HashMap.insert n (Set.union use' (Set.difference (liveOut ! n) def')) liveIn
        liveOut' = HashMap.insert n (Set.unions $ map (liveIn !) succ') liveOut

interferenceGraph :: FGraph.FlowGraph -> LiveMap -> IGraph
interferenceGraph fGraph liveIn = IGraph { graph = igraph } 
  where igraph = foldl f AdjMap.empty $ FGraph.nodes fGraph
        def' = FGraph.def fGraph
        f acc n = overlay (edges [(d, t) | d <- def' ! n,
                                  t <- Set.toList $ liveIn ! n]) acc

arm1 = MC_ (ARM.MOV AL) (R0) (R R0) 
instr = IOPER { assem = arm1, src = [], dst = [0], jump = [] }
testLivenessInstrs1 = [instr, instr, instr, instr, instr, instr] 
testUse1 = HashMap.fromList [(1, []),
            (2, [0]),
            (3, [1,2]),
            (4, [1]),
            (5, [1]),
            (5, [0]),
            (6, [2])]

testDef1 = HashMap.fromList [(1, [0]),
            (2, [1]),
            (3, [2]),
            (4, [0]),
            (5, []),
            (6, [])]
           
testEdges1 = [(1,2), (2, 3), (3, 4), (4, 5), (5,2), (5, 6)]
testFlowGraph1 =
  FGraph.newFlowGraph { FGraph.def = testDef1,
                        FGraph.use = testUse1,
                        FGraph.control = AdjMap.edges testEdges1 }
