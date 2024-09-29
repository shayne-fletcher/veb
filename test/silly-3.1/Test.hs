import Control.Exception
import Control.Monad (forM_, replicateM)
import Data.Bifunctor (bimap)
import Data.List hiding (delete, insert)
import Data.List qualified (delete)
import Data.Maybe
import Data.Set qualified
import Silly_3_1
import System.Environment
import System.Random (randomRIO)
import Test.Tasty
import Test.Tasty.HUnit as HUnit
import Prelude hiding (succ)

main :: IO ()
main = do
  setEnv "TASTY_NUM_THREADS" "1"
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    " All tests"
    [ testCase "make" $ forM_ [0 .. 16] makeTest,
      testCase "mark" $ forM_ [{- h= -} 2 ^ k | k <- [0 .. 4] :: [Int]] markTest,
      testCase "toNum" $ forM_ [{- h= -} 2 ^ k | k <- [0 .. 4] :: [Int]] toNumTest,
      testCase "setEmpty" $ forM_ [{- h= -} 2 ^ k | k <- [0 .. 4] :: [Int]] setEmptyTest,
      testCase "setMin" $
        sequence_
          [ setMinTest 2 [1, 2],
            setMinTest 12 [4095, 2048, 1024, 512]
          ],
      testCase "setMax" $
        sequence_
          [ setMaxTest 2 [1, 2],
            setMaxTest 12 [4095, 2048, 1024, 512]
          ],
      testCase "setPredSuccTest" $
        sequence_
          [ setPredSuccTest 1 [] 1 (Nothing, Nothing),
            setPredSuccTest 1 [1] 2 (Just 1, Nothing),
            setPredSuccTest 3 [3, 4, 5, 6, 7] 5 (Just 4, Just 6),
            setPredSuccTest 3 [1, 2, 3, 4, 5, 6, 7, 8] 1 (Nothing, Just 2)
          ],
      testCase "setExtractMinTest" $ setExtractMinTest 12 [4095, 2048, 1024, 512],
      testCase "setExtractMaxTest" $ setExtractMaxTest 12 [4095, 2048, 1024, 512]
    ]

makeTest :: Int -> IO ()
makeTest h = do
  let t = make (Control.Exception.assert (h >= 0) h)
      n = 2 ^ h
      ls = postorder f [] (top t)
      num_nodes = postorder (\acc _ -> acc + 1) (0 :: Int) (top t)
  assertEqual "2^h = # leaves" (length ls) n
  assertEqual "2^(h + 1) - 1 = # nodes" num_nodes (2 ^ (h + 1) - 1)
  where
    f acc n = case n of (Node {}, _) -> acc; (Leaf {}, _) -> n : acc

markTest :: Int -> Assertion
markTest h = do
  let n = 2 ^ h :: Int
      t0 = make (Control.Exception.assert (h >= 0) h)
      k = h

  is <- selectK n k

  let t = foldl' (setInsert h) t0 is
      ls = leaves t
  assertBool "expected marks" $
    and [getMark (fst (at ls (i - 1) n)) | i <- is]
  assertBool "no unexpected marks" $
    not . or $ [getMark . fst $ at ls (i - 1) n | i <- [1 .. n], i `notElem` is]

  let t' = foldl' (delete h) t is
      ls' = leaves t'
  assertBool "expected marks" $
    and [not . getMark $ fst (at ls' (i - 1) n) | i <- is]
  assertBool "no unexpected marks" $
    not . or $ [getMark . fst $ at ls' (i - 1) n | i <- [1 .. n], i `notElem` is]
  where
    at :: [Loc] -> Int -> Int -> Loc
    at ls i n = ls !! Control.Exception.assert (i >= 0 && i < n) i

    selectK :: Int -> Int -> IO [Int]
    selectK n k = do
      cs <- replicateM k (randomRIO (1, n))
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
  assertEqual "leaf value matches its' index" ([1 .. n] :: [Int]) ns

setEmptyTest :: Int -> Assertion
setEmptyTest h = do
  let t = make (Control.Exception.assert (h >= 0) h)
      t' = setInsert h t 1
  assertBool "setEmpty" $ setEmpty t
  assertBool "not setEmpty" $ not (setEmpty t')

setMinTest :: Int -> [Int] -> IO ()
setMinTest h ls = do
  let n = 2 ^ h :: Int
      t0 = make (Control.Exception.assert (h >= 0) h)
      t = foldl' (setInsert h) t0 (Control.Exception.assert (and [l >= 1 && l <= n | l <- ls]) ls)
  assertEqual "min element matches expected value" (minimum ls) (toNum . Data.Maybe.fromJust . setMin $ t)

setMaxTest :: Int -> [Int] -> IO ()
setMaxTest h ls = do
  let n = 2 ^ h :: Int
      t0 = make (Control.Exception.assert (h >= 0) h)
      t = foldl' (setInsert h) t0 (Control.Exception.assert (and [l >= 1 && l <= n | l <- ls]) ls)
  assertEqual "max element matches expected value" (maximum ls) (toNum . Data.Maybe.fromJust . setMax $ t)

setPredSuccTest :: Int -> [Int] -> Int -> (Maybe Int, Maybe Int) -> IO ()
setPredSuccTest h ls j expect = do
  let n = 2 ^ h :: Int
      t0 = make (Control.Exception.assert (h >= 0) h)
      t = foldl' (setInsert h) t0 (Control.Exception.assert (and [l >= 1 && l <= n | l <- ls]) ls)
      loc = path (Control.Exception.assert (1 <= j && j <= n) j) h t
  assertEqual "pred/succ match expectations" expect (bimap maybeToNum maybeToNum (setPred loc, setSucc loc))
  where
    maybeToNum = (toNum <$>)

setExtractMinTest :: Int -> [Int] -> IO ()
setExtractMinTest h ls = do
  let n = 2 ^ h :: Int
      t0 = make (Control.Exception.assert (h >= 0) h)
      t = foldl' (setInsert h) t0 (Control.Exception.assert (and [l >= 0 && l < n | l <- ls]) ls)
      t' = setExtractMin t
  assertEqual "min element matches expected value" (minimum (Data.List.delete (minimum ls) ls)) (toNum . Data.Maybe.fromJust . setMin $ t')

setExtractMaxTest :: Int -> [Int] -> IO ()
setExtractMaxTest h ls = do
  let n = 2 ^ h :: Int
      t0 = make (Control.Exception.assert (h >= 0) h)
      t = foldl' (setInsert h) t0 (Control.Exception.assert (and [l >= 0 && l < n | l <- ls]) ls)
      t' = setExtractMax t
  assertEqual "max element matches expected value" (maximum (Data.List.delete (maximum ls) ls)) (toNum . Data.Maybe.fromJust . setMax $ t')
