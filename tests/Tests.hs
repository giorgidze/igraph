module Main where

import Data.SGL
import Test.QuickCheck
import Data.List
import qualified Data.Set as S
import Control.Monad

instance (Arbitrary nl, Arbitrary el) => Arbitrary (Edge nl el) where
  arbitrary = liftM3 edge arbitrary arbitrary arbitrary 

noDupEdges :: (Eq nl) => [Edge nl el] -> Bool
noDupEdges eds = length eds == (length $ nub $ map (\e -> (edgeFrom e, edgeTo e)) eds)

sameElements :: (Eq a) => [a] -> [a] -> Bool
sameElements as bs = null (as \\ bs) && null (bs \\ as)

prop_make_graph_edges :: [Edge Integer Integer] -> Property
prop_make_graph_edges eds = noDupEdges eds ==> sameElements eds (edges (makeGraph eds))

prop_make_graph_nodes :: [Edge Integer Integer] -> Bool
prop_make_graph_nodes eds = sameElements  (nodes (makeGraph eds)) (nub (concatMap (\e -> [edgeFrom e, edgeTo e]) eds))

qc :: Testable prop => prop -> IO ()
qc = quickCheckWith stdArgs{maxSuccess = 10000}

main :: IO ()
main = do
  putStrLn "prop_make_graph_edges"
  qc prop_make_graph_edges
  
  putStrLn "prop_make_graph_nodes"
  qc prop_make_graph_nodes