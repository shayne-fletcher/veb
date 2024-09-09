import Control.Exception
import Data.Maybe
import Silly_3_1

main :: IO ()
main = do
  let h = 16
      t = make h
      n = 2 ^ h
      val = 65534
      t' = fst . upmost . mark $ path val h t
      ls = leaves t'
      len = length ls
  assert (getMark (fst (ls !! val))) (pure ())
  putStrLn $ "n = " <> assert (len == n) (show n)
  putStrLn $ "root t' " <> if marked t' then "is marked" else "is not " <> "marked"
  -- putStrLn $ show t'

  print (map toNum ls)
  assert (fromJust (minElem t') == path val h t') (pure ())
  putStrLn $ "min = " <> show (toNum (fromJust (minElem t')))

  print (map toNum (harvestLeft (path 5 h t')))
  print (map toNum (harvestRight (path 65533 h t')))
