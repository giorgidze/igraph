module Main where

import Data.IGraph

main :: IO ()
main = do
  let gr = make [("1","2"),("2","1"),("3","2"),("4","5")]
  print (nodeNumber gr)
  print (edgeNumber gr)
  print (betweenness  gr)
  print (eigenvectorCentrality  gr)
  print (cluster gr)
  print (shortestPathsIn gr "1")
  return ()