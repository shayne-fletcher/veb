import Control.Exception
import Data.Maybe
import Silly_3_1

main :: IO ()
main = do
  let h = 16
      t = make h
      n = 2 ^ h
      val = 65535
      t' = fst . upmost . mark $ path val h t
      ls = leaves t'
      len = length ls
  assert (getMark (fst (ls !! (val - 1)))) (pure ())
  putStrLn $ "n = " <> assert (len == n) (show n)
  putStrLn $ "root t' " <> if getMark t' then "is marked" else "is not " <> "marked"
  -- putStrLn $ show t'

  print (map toNum ls)
  assert (fromJust (setMin t') == path val h t') (pure ())
  putStrLn $ "min = " <> show (toNum (fromJust (setMin t')))

  let s = path 65535 h t'
      sInBits = foldl' (<>) [] $ reverse (map show (toBits s))

  putStrLn $ show (toBits s)
  putStrLn sInBits

  print (map toNum (harvestLeft (path 5 h t')))
  print (map toNum (harvestRight (path 65532 h t')))
