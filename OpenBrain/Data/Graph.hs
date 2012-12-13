module OpenBrain.Data.Graph(
  Vertex, Edge, Graph, buildG
, vertices, edges
, next, next', prev, prev'
, edgesFrom, edgesTo, hasEdge
)where

import Data.Array ((!))
import Data.Graph (Vertex, Edge)
import Data.List  (nub)
import qualified Data.Array as A
import qualified Data.Graph as G

data Graph = Graph {      -- | My own Graph type only encapsules two Data.Graph
    simple     :: G.Graph -- | A Data.Graph contained in my wrapper
  , transposed :: G.Graph -- | The transposed version of the simple field for reverse lookups.
  }

{-| Creates a Graph from a list of edges. |-}
buildG :: [Edge] -> Graph
buildG edges =
  let vs     = concatMap (\(v1, v2) -> [v1, v2]) edges
      bounds = (minimum vs, maximum vs)
      graph  = G.buildG bounds edges
  in Graph {
    simple     = graph
  , transposed = G.transposeG graph
  }

{-| All vertices in the graph. |-}
vertices :: Graph -> [Vertex]
vertices = G.vertices . simple

{-| All edges in the graph. |-}
edges :: Graph -> [Edge]
edges = G.edges . simple

{-| A list of Vertices that can be reached in a Graph from a given Vertex. |-}
next :: Graph -> Vertex -> [Vertex]
next = (!) . simple

next' :: Graph -> [Vertex] -> [Vertex]
next' g = nub . concatMap (next g)

{-| A list of Vertices that can reach a given Vertex in a Graph. |-}
prev :: Graph -> Vertex -> [Vertex]
prev = (!) . transposed

prev' :: Graph -> [Vertex] -> [Vertex]
prev' g = nub . concatMap (prev g)

{-| All edges in the Graph that have the given Vertex as first element. |-}
edgesFrom :: Graph -> Vertex -> [Edge]
edgesFrom g v = zip (repeat v) $ next g v

{-| All edges in the Graph that have the given Vertex as second element. |-}
edgesTo :: Graph -> Vertex -> [Edge]
edgesTo g v = zip (prev g v) $ repeat v

{-| Tests if a given Edge belongs to the graph. |-}
hasEdge :: Graph -> Edge -> Bool
hasEdge g (v1, v2) = v2 `elem` next g v1

