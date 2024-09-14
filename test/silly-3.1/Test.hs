import Control.Exception
import Control.Monad (forM_, replicateM)
import Data.Maybe
import Data.Set qualified
import Silly_3_1
import System.Environment
import System.Random (randomRIO)
import Test.Tasty
import Test.Tasty.HUnit as HUnit

main :: IO ()
main = do
  setEnv "TASTY_NUM_THREADS" "1"
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    " All tests"
    [ testCase "make" $ forM_ [0 .. 16] makeTest,
      testCase "mark" $ forM_ [2 ^ i | i <- [0 .. 4] :: [Int]] markTest,
      testCase "toNum" $ forM_ [2 ^ i | i <- [0 .. 4] :: [Int]] toNumTest,
      testCase "setEmpty" $ forM_ [2 ^ i | i <- [0 .. 4] :: [Int]] setEmptyTest,
      testCase "setMinElem (1)" $ setMinElemTest 2 [1, 2],
      testCase "setMinElem (2)" $ setMinElemTest 12 [4095, 2048, 1024, 512],
      testCase "setMaxElem (1)" $ setMaxElemTest 2 [1, 2],
      testCase "setMaxElem (2)" $ setMaxElemTest 12 [4095, 2048, 1024, 512]
    ]

makeTest :: Int -> IO ()
makeTest h = do
  let t = make (Control.Exception.assert (h >= 0) h)
      n = 2 ^ h
      ls = postorder f [] (top t)
      num_nodes = postorder (\acc _ -> acc + 1) (0 :: Int) (top t)
  HUnit.assertEqual "2^h = # leaves" (length ls) n
  HUnit.assertEqual "2^(h + 1) - 1 = # nodes" num_nodes (2 ^ (h + 1) - 1)
  where
    f acc n = case n of (Node {}, _) -> acc; (Leaf {}, _) -> n : acc

markTest :: Int -> Assertion
markTest h = do
  let n = 2 ^ h :: Int
      t0 = make (Control.Exception.assert (h >= 0) h)
      k = h

  is <- selectK n k

  let t = foldl' (insert h) t0 is
      ls = leaves t
  assertBool "expected marks" $
    and [getMark (fst (at ls i n)) | i <- is]
  assertBool "no unexpected marks" $
    not . or $
      [getMark $ fst (at ls i n) | i <- [0 .. n - 1], i `notElem` is]

  let t' = foldl' (delete h) t is
      ls' = leaves t'
  assertBool "expected marks" $
    and [not . getMark $ fst (at ls' i n) | i <- is]
  assertBool "no unexpected marks" $
    not . or $
      [getMark $ fst (at ls' i n) | i <- [0 .. n - 1], i `notElem` is]
  where
    at :: [Loc] -> Int -> Int -> Loc
    at ls i n = ls !! Control.Exception.assert (i >= 0 && i < n) i

    selectK :: Int -> Int -> IO [Int]
    selectK n k = do
      cs <- replicateM k (randomRIO (0, n - 1))
      let is = Data.Set.fromList cs
      case Data.Set.size is of
        len | len == k -> pure $ Data.Set.toList is
        _ -> selectK n k -- In case of duplicates.

toNumTest :: Int -> Assertion
toNumTest h = do
  let t = make (Control.Exception.assert (h >= 0) h)
      n = 2 ^ h
      ls = leaves t
      ns = map toNum ls :: [Int]
  assertEqual "leaf value matches its' index" ns ([0 .. n - 1] :: [Int])

setEmptyTest :: Int -> Assertion
setEmptyTest h = do
  let t = make (Control.Exception.assert (h >= 0) h)
      t' = insert h t 0
  assertBool "setEmpty" $ setEmpty t
  assertBool "not setEmpty" $ not (setEmpty t')

setMinElemTest :: Int -> [Int] -> IO ()
setMinElemTest h ls = do
  let n = 2 ^ h :: Int
      t0 = make (Control.Exception.assert (h >= 0) h)
      t = foldl' (setInsert h) t0 (Control.Exception.assert (and [l >= 0 && l < n | l <- ls]) ls)
  assertEqual "min element matches expected value" (toNum . Data.Maybe.fromJust . setMinElem $ t) (minimum ls)

setMaxElemTest :: Int -> [Int] -> IO ()
setMaxElemTest h ls = do
  let n = 2 ^ h :: Int
      t0 = make (Control.Exception.assert (h >= 0) h)
      t = foldl' (setInsert h) t0 (Control.Exception.assert (and [l >= 0 && l < n | l <- ls]) ls)
  assertEqual "max element matches expected value" (toNum . Data.Maybe.fromJust . setMaxElem $ t) (maximum ls)
