module Graph where
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Datatypes
type Vertex = Int -- Vertex is an integer
type Edge = (Vertex, Vertex) -- Edge is a tuple of vertexes
type Graph = Map Vertex (Set Vertex) 
    -- Graph is a map of vertexes to sets of vertexes (essentially an adjacency list)

-- Creates an empty graph
initGraph :: Graph
initGraph = Map.empty

-- Adds a vertex to the given graph, takes care of duplicates
addVertex :: Vertex -> Graph -> Graph
addVertex v g = 
    case Map.lookup v g of
        Nothing -> Map.insert v Set.empty g
        Just _ -> g

-- Adds an edge to the given graph, takes care of duplicates
addEdge :: Edge -> Graph -> Graph
addEdge (v, w) g = result
    where 
        g' = addVertex v (addVertex w g)
        result = Map.insert v (Set.insert w (neighbors v g')) g' 

-- Deletes a given vertex, checks to make sure the verex exists
delVertex :: Vertex -> Graph -> Graph
delVertex  = Map.delete 

-- Deletees a given edge, checks to make sure the edge exists
delEdge :: Edge -> Graph -> Graph
delEdge (v, w) g = 
    case Map.lookup v g of
        Nothing -> g
        Just _ -> result
        where
            result = Map.insert v (Set.delete w (neighbors v g)) g

-- Returns the neighbors of the given vertex
neighbors :: Vertex -> Graph -> Set Vertex
neighbors v g = g Map.! v

-- Returns all reachable nodes from the given vertex
reachable :: Vertex  -> Graph -> [Vertex]
reachable v g = reverse (foldl search [] (neighbors v g))
    where
        search visited v = 
            if v `elem` visited
            then visited
            else foldl search (v:visited) (neighbors v g)

-- Returns true if the second vertex is reachable from the first
path :: Vertex -> Vertex -> Graph -> Bool
path v w g =  w `elem` reachable v g


