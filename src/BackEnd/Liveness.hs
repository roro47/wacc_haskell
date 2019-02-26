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
def n fGraph = Set.fromList (FGraph.use fGraph ! n)

succ :: Int -> FGraph.FlowGraph -> [Int] 
succ n fGraph = Set.toList (postSet n (FGraph.control fGraph))

type LiveMap = HashMap.Map Int (Set.Set Temp.Temp)


testLiveness :: [Assem.Instr] -> (LiveMap, LiveMap)
testLiveness instrs = calcLiveness fgraph instrs
  where fgraph = instrsToGraph instrs

-- given flow graph, calculate live map
-- live map: given a node, return the set of node that are live at that point
-- return (liveIn, liveOut)
calcLiveness :: FGraph.FlowGraph -> [Assem.Instr] -> (LiveMap, LiveMap)
calcLiveness fgraph instrs = calcLiveness' nodes fgraph liveIn liveOut
  where nodes = reverse [0..length instrs-1]
        -- assign nodes to empty set
        liveIn =  foldl (\acc x -> HashMap.insert x Set.empty acc) HashMap.empty nodes
        liveOut = liveIn

-- update liveIn and liveOut iteratively, stop until liveIn and liveOut stop changing
calcLiveness' :: [Int] -> FGraph.FlowGraph ->  LiveMap -> LiveMap -> (LiveMap, LiveMap)
calcLiveness' nodes fgraph liveIn liveOut
  | liveIn == liveIn' && liveOut == liveOut' = (liveIn, liveOut)
  | otherwise = calcLiveness' nodes fgraph liveIn' liveOut'
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
testLivenessInstrs1 =
  [IOPER { assem = arm1, src = [], dst = [0], jump = [] },
   ILABEL { assem = arm1, lab = "label1"},
   IOPER { assem = arm1, src = [0], dst = [1], jump = [] },
   IOPER { assem = arm1, src = [1,2], dst = [2], jump = [] },
   IOPER { assem = arm1, src = [1], dst = [0], jump = [] },
   IOPER { assem = arm1, src = [0], dst = [], jump = ["label1", "label2"] },
   ILABEL { assem = arm1, lab = "label2"},
   IOPER { assem = arm1, src = [2], dst = [], jump = [] } ]
   
