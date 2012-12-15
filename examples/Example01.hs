import Data.IGraph
import Data.List

{-

Graphs:

g: 0 --- 1 --- 2 --- 3 --- 4 --- 5
    \               / \         /
     `-------------´   `-------´

h: 0 --- 1 --- 2 --- 3   4 --- 5 --- 6
    \               /     \         /
     `-------------´       `-------´

           .----2---.
          /          \
w: 0 -1- 1 -3- 2 -1- 3
    \               /
     `------1------´

-}

g,h :: Graph D Int
g = fromList [(0,1), (1,2), (2,3), (3,4), (4,5), (0,3), (3,5)]
h = fromList [(0,1), (1,2), (2,3), (0,3), (4,5), (5,6), (4,6)]

w :: Graph (Weighted D) Int
w = fromListWeighted [ (0,1, 1), (1,2, 3), (1,3, 1), (3,2, 1), (1,3, 2), (0,3, 1) ]

(~>) :: a -> b -> (a,b)
a ~> b = (a,b)

main :: IO ()
main = do
  putStrLn $ "Nodes:\n\t"                       ++ show (nodes g)
  putStrLn $ "Edges:\n\t"                       ++ show (edges g)
  putStrLn $ "0 and 3 are connected:\n\t"       ++ show (areConnected g 0 3)
  putStrLn $ "0 and 5 are connected:\n\t"       ++ show (areConnected g 0 5)
  putStrLn $ "Graph is connected:\n\t"          ++ show (isConnected g Weak)
  putStrLn $ "Shortest path from 0 to 5:\n\t"   ++ show (getShortestPath g 0 5)
  putStrLn $ "Biconnected components of g:\n\t" ++ intercalate "\n\t" (map show . (\(_i,_t,ce,_c,_a) -> ce) $ biconnectedComponents g)
  putStrLn $ "Subgraph containing [3,4,5]:\n\t" ++ show (inducedSubgraph g (VsList [3,4,5]) SubgraphAuto)
  putStrLn $ "h decomposed (weakly):\n\t"       ++ intercalate "\n\t" (map show (decompose h Weak (-1) 0))

pagerankExample :: (Double, [(Char, Double)])
pagerankExample = pagerank graph VsAll 0.85
 where
  graph :: Graph D Char
  graph = fromList
        [ 'A' ~> 'B'
        , 'A' ~> 'C'
        , 'B' ~> 'C'
        , 'C' ~> 'A'
        , 'D' ~> 'C' ]
