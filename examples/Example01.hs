import Data.IGraph

{-

Graph:

  0 --- 1 --- 2 --- 3 --- 4 --- 5
   \               / \         /
    `-------------´   `-------´

-}

g :: Graph D Int
g = fromList $ [(0,1), (1,2), (2,3), (3,4), (4,5), (0,3), (3,5)]

main :: IO ()
main = do
  putStrLn $ "Nodes:                     " ++ show (evalIGraph g $ nodes)
  putStrLn $ "Edges:                     " ++ show (evalIGraph g $ edges)
  putStrLn $ "0 and 3 are connected:     " ++ show (evalIGraph g $ areConnected 0 3)
  putStrLn $ "0 and 5 are connected:     " ++ show (evalIGraph g $ areConnected 0 5)
  putStrLn $ "Graph is connected:        " ++ show (evalIGraph g $ isConnected Weak)
  putStrLn $ "Shortest path from 0 to 5: " ++ show (evalIGraph g $ getShortestPath 0 5 Out)
