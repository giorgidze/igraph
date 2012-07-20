import Data.IGraph

{-

Graph:

  0 --- 1 --- 2 --- 3 --- 4 --- 5
   \               / \         /
    `-------------´   `-------´

-}

g :: Graph U Int
g = fromList $ [(0,1), (1,2), (2,3), (3,4), (4,5), (0,3), (3,5)]

main :: IO ()
main = do
  putStrLn $ "Nodes:                     " ++ show (nodes g)
  putStrLn $ "Edges:                     " ++ show (edges g)
  putStrLn $ "0 and 3 are connected:     " ++ show (areConnected g 0 3)
  putStrLn $ "0 and 5 are connected:     " ++ show (areConnected g 0 5)
  putStrLn $ "Graph is connected:        " ++ show (isConnected g Weak)
  putStrLn $ "Shortest path from 0 to 5: " ++ show (getShortestPath g 0 5 Out)
