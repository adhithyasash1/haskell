import Data.Set (Set)
import qualified Data.Set as Set

type Graph = [(Char, [Char])]

dfs :: Graph -> Char -> Set Char
dfs graph start = dfsHelper Set.empty start
  where
    dfsHelper visited node
      | node `Set.member` visited = visited
      | otherwise = foldl dfsHelper (Set.insert node visited) (neighbors node)
    
    neighbors node = case lookup node graph of
      Just neighs -> neighs
      Nothing     -> []

main :: IO ()
main = do
    let graph = [('A', ['B', 'C']),
                 ('B', ['D', 'E']),
                 ('C', ['F']),
                 ('D', []),
                 ('E', ['F']),
                 ('F', [])]
    print $ dfs graph 'A'


import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type WeightedGraph = Map Char [(Char, Int)]

dijkstra :: WeightedGraph -> Char -> Map Char Int
dijkstra graph start = dijkstraHelper (Map.singleton start 0) (Set.singleton start)
  where
    dijkstraHelper dists visited
      | Set.null unvisited = dists
      | otherwise = dijkstraHelper newDists newVisited
      where
        unvisited = Map.keysSet dists `Set.difference` visited
        minNode = Set.findMin unvisited
        currentDist = dists Map.! minNode
        neighbors = Map.findWithDefault [] minNode graph
        newDists = foldl updateDist dists neighbors
        newVisited = Set.insert minNode visited
        
        updateDist ds (node, weight) =
          let newDist = currentDist + weight
           in case Map.lookup node ds of
                Just oldDist -> if newDist < oldDist
                                then Map.insert node newDist ds
                                else ds
                Nothing      -> Map.insert node newDist ds

main :: IO ()
main = do
    let graph = Map.fromList [('A', [('B', 3), ('C', 5)]),
                              ('B', [('C', 1), ('D', 2)]),
                              ('C', [('D', 4)]),
                              ('D', [])]
    print $ dijkstra graph 'A'
