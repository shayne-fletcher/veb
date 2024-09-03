{-# OPTIONS_GHC -Wno-unused-top-binds #-}
import Control.Exception (assert)
import Data.Bits
import Debug.Trace

data Tree = Leaf Bool | Node Tree Tree Bool
  deriving (Show)

height :: Tree -> Int
height (Leaf _) = 0
height (Node l _ _) = height l + 1

marked :: Tree -> Bool
marked (Leaf True) = True
marked (Node _ _ True) = True
marked _ = False

-- Huet zipper. See
-- http://www.st.cs.uni-sb.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf
-- and https://wiki.haskell.org/Zipper

data Ctx = Top | L Ctx Tree | R Tree Ctx
  deriving (Show)

type Loc = (Tree, Ctx)

getMark :: Tree -> Bool
getMark (Leaf m) = m
getMark (Node _ _ m) = m

left :: Loc -> Loc
left (Node l r _, c) = (l, L c r)
left (Leaf _, _) = error "`left` applied to `Leaf`"

right :: Loc -> Loc
right (Node l r _, c) = (r, R l c)
right (Leaf _, _) = error "`right` applied to `Leaf`"

top :: Tree -> Loc
top t = (t, Top)

up :: Loc -> Loc
up (t, L c r) = (Node t r (getMark t || getMark r), c)
up (t, R l c) = (Node l t (getMark l || getMark t), c)
up (_, Top) = error "`up` applied to location containing `Top`"

upmost :: Loc -> Loc
upmost loc@(_, Top) = loc
upmost loc = upmost (up loc)

_modify :: Loc -> (Tree -> Tree) -> Loc
_modify (t, c) f = (f t, c)

mark :: Loc -> Loc
mark (Leaf _, c) = (Leaf True, c)
mark (Node l r _, c) = (Node l r True, c)

make :: Int -> Tree
make h =
  let (t, n) = make_rec (Debug.Trace.trace ("h = " <> show h) h)
  in assert (height t == h) (assert (n == 2^(h + 1) - 1)
    (Debug.Trace.trace ("tree of " <> show n <> " nodes") t))
  where
    make_rec :: Int -> (Tree, Int)
    make_rec level =
      case level of
        0 -> (Leaf False, 1)
        _ -> (Node l r False, 1 + c1 + c2)
          where
            (l, c1) = make_rec (level - 1)
            (r, c2) = make_rec (level - 1)

path :: Int -> Int -> (Tree -> Loc)
path entry h = path_rec top (assert(entry >= 0 && entry < 2^h) entry) 0
  where
    path_rec :: (Tree -> Loc) -> Int -> Int -> (Tree -> Loc)
    path_rec acc n i =
       if i == h then
        acc
      else
        path_rec (dir n i h . acc) n (i + 1)
    -- In `dir x i n`, `x` has length `n` bits, `i` is the bit under
    -- consideration. e.g. when n = 4, x can represent values 0..15.
    -- If x = 13 (0b1101) say, we compute `right . right . left .
    -- right`
    dir :: Int -> Int -> Int -> (Loc -> Loc)
    dir x i n =
      case (x `shiftL` i .&. mask) `shiftR` k of
        0 -> left
        1 -> right
        _ -> error "a match on an impossible case"
      where
        k = n - 1
        mask = 1 `shiftL` k

postorder :: (a -> Tree -> a) -> a -> Tree -> a
postorder f acc n@(Node l r _) =
  let acc' = postorder f acc l
      acc'' = postorder f acc' r
      acc''' = f acc'' n
  in acc'''
postorder f acc n@(Leaf _) = f acc n

main :: IO ()
main = do
  let h = 16
      t = make h
      n = 2^h
      val = 65534
      t' = fst . upmost . mark $ path val h t
      leaves = reverse $ postorder f [] t'
      len = length leaves
  assert (getMark (leaves!!val)) (pure ())
  putStrLn $ "n = " <> assert (len == n) (show n)
  putStrLn $ "root t' " <> if marked t' then "" else "not " <> "marked"
  putStrLn $ show t'
  putStrLn $ show leaves
  where
    f acc n = case n of Node {} -> acc; Leaf {} -> n : acc

{-
{-# LANGUAGE RecordWildCards #-}

import Data.List
import Data.Maybe
import Debug.Trace

data Node = Nil
  | Node {
      parent:: Maybe Node,
      left :: Node,
      right :: Node,
      marked :: Bool
    }
  deriving(Eq)

make :: Int -> Node
make h = make_rec (h + 1) Nothing
  where
    make_rec :: Int -> Maybe Node -> Node
    make_rec level p =
      case level of
        0 -> Nil
        _ ->
          let n =
                Node {
                  parent = p,
                  left = make_rec (level - 1) (Just n),
                  right = make_rec (level - 1) (Just n),
                  marked = False
                }
          in n

inorder :: (a -> Node -> a) -> a -> Node -> a
inorder f acc n@Node { parent = _, left = l, right = r, marked = _} = f (inorder f (inorder f acc l) r) n
inorder f acc n@Nil = f acc n

numEdges n Node { parent = Nothing } = 0 :: Int
numEdges n Node { parent = Just p } = numEdges n p + 1

main = do
  let k = 4
      t = make k
      leaves = inorder f [] t
  putStrLn ("number leaves: " <> show (length leaves))
  putStrLn ("num edges of 15: " <> show (numEdges 0 (leaves!!12)))

  pure ()
  where
    f acc n =
      case n of
        Node { parent=_, left = Nil, right = Nil } -> n : acc
        _ -> acc
-}
