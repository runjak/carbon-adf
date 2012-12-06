module OpenBrain.Data.Graph(
  Vertex, Edge, Graph
, buildG, vertices, edges
, edgesFrom, edgesTo
, next, prev
)where

import Control.Arrow ((***))
import Control.Monad (join, liftM2)
import Data.BitArray (BitArray)
import Data.Map      (Map)
import Data.Maybe
import qualified Data.BitArray as BA
import qualified Data.List     as List
import qualified Data.Map      as Map

type Vertex = Int
type Edge   = (Vertex, Vertex)

type SqSize = Int
data Graph  = Graph {
    array  :: BitArray
  , sqSize :: SqSize -- | Length of sqrt(|array|)
  }

{-| Converts an Edge to an Int fitting the BitArray. |-}
fromEdge :: SqSize -> Edge -> Int
fromEdge s (v1, v2) = v1 * s + v2

fromEdge' :: Graph -> Edge -> Int
fromEdge' = fromEdge . sqSize

{-| Converts an Int from the BitArray to an Edge. |-}
toEdge :: SqSize -> Int -> Edge
toEdge s i = i `divMod` s

toEdge' :: Graph -> Int -> Edge
toEdge' = toEdge . sqSize

{-| Builds a Graph and two map functions from a list of Edges of Ord. |-}
buildG :: Ord a => [(a, a)] -> (Graph, Vertex -> a, a -> Maybe Vertex)
buildG edges =
  let vs     = List.nub . List.sort $ concatMap (\(v1, v2) -> [v1, v2]) edges
      size   = length vs
      bounds = (0, size^2)
      nvMap  = Map.fromList $ zip [0..] vs
      vnMap  = Map.fromList $ zip vs [0..]
      tBits  = flip zip (repeat True) . map (fromEdge size)
             $ translateEdges vnMap edges
      graph  = Graph {
               array  = BA.bitArray bounds tBits
             , sqSize = size}
  in (graph, fromJust . flip Map.lookup nvMap, flip Map.lookup vnMap)
  where
    translateEdges :: Ord a => Map a Int -> [(a, a)] -> [(Int, Int)]
    translateEdges m = map . join (***) $ fromJust . flip Map.lookup m

{-| All vertices in the graph. |-}
vertices :: Graph -> [Vertex]
vertices g = [0..(subtract 1 $ sqSize g)]

{-| Tests if a given Edge belongs to the graph. |-}
hasEdge :: Graph -> Edge -> Bool
hasEdge g e = BA.lookupBit (array g) $ fromEdge' g e

{-| All edges in the graph. |-}
edges :: Graph -> [Edge]
edges g = filter (hasEdge g) . join (liftM2 (,)) $ vertices g

{-| All edges in the Graph that have the given Vertex as first element. |-}
edgesFrom :: Graph -> Vertex -> [Edge]
edgesFrom g v = filter (hasEdge g) . zip (repeat v) $ vertices g

{-| All edges in the Graph that have the given Vertex as second element. |-}
edgesTo :: Graph -> Vertex -> [Edge]
edgesTo g v = filter (hasEdge g) . zip (vertices g) $ repeat v

{-| A list of Vertices that can be reached in a Graph from a given Vertex. |-}
next :: Graph -> Vertex -> [Vertex]
next g = map snd . edgesFrom g

{-| A list of Vertices that can reach a given Vertex in a Graph. |-}
prev :: Graph -> Vertex -> [Vertex]
prev g = map fst . edgesTo g

