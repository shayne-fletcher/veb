{-# OPTIONS_GHC -Wno-unused-imports #-}

import Control.Exception
import Control.Monad (forM_)
import System.Environment
import Test.Tasty
import Test.Tasty.HUnit as HUnit

import Silly_3_1

tests :: TestTree
tests = testGroup " All tests"
  [
    testCase "make" $ forM_ [0..16] $ \h -> test_make h
  ]

test_make :: Int -> IO ()
test_make h = do
  let
     t = make (Control.Exception.assert (h >= 0) h)
     n = 2^h
     leaves = postorder f [] (top t)
     num_nodes = postorder (\acc _ -> acc + 1) (0 :: Int) (top t)
  HUnit.assertEqual "2^h = # leaves" (length leaves) n
  HUnit.assertEqual "2^(h + 1) - 1 = # nodes" num_nodes (2^(h + 1) - 1)
  where
    f acc n = case n of (Node {}, _) -> acc; (Leaf {}, _) -> n : acc

main :: IO ()
main = do
  setEnv "TASTY_NUM_THREADS" "1"
  defaultMain tests
