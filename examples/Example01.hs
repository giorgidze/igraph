import Data.IGraph

{-

Graphs:

g: 0 --- 1 --- 2 --- 3 --- 4 --- 5
    \               / \         /
     `-------------´   `-------´

h: 0 --- 1 --- 2 --- 3   4 --- 5 --- 6
    \               /     \         /
     `-------------´       `-------´

w: 0 -1- 1 -3- 2 -1- 3
    \               /
     `------1------´

-}

g,h :: Graph D Int
g = fromList [(0,1), (1,2), (2,3), (3,4), (4,5), (0,3), (3,5)]
h = fromList [(0,1), (1,2), (2,3), (0,3), (4,5), (5,6), (4,6)]

w :: Graph (Weighted D) Int
w = fromListWeighted [ (0,1, 1), (1,2, 3), (1,3, 1), (3,2, 1) ]

main :: IO ()
main = do
  putStrLn $ "Nodes:                       " ++ show (nodes g)
  putStrLn $ "Edges:                       " ++ show (edges g)
  putStrLn $ "0 and 3 are connected:       " ++ show (areConnected g 0 3)
  putStrLn $ "0 and 5 are connected:       " ++ show (areConnected g 0 5)
  putStrLn $ "Graph is connected:          " ++ show (isConnected g Weak)
  putStrLn $ "Shortest path from 0 to 5:   " ++ show (getShortestPath g 0 5)
  putStrLn $ "Subgraph containing [3,4,5]: " ++ show (inducedSubgraph g (VsList [3,4,5]) SubgraphAuto)
