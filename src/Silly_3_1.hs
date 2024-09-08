module Silly_3_1 where

import Control.Exception (assert)
import Data.Bits

data Tree = Leaf Bool | Node Tree Tree Bool
  deriving (Show, Eq)

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
  deriving (Show, Eq)

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
  let (t, n) = make_rec h
   in assert
        (height t == h)
        ( assert
            (n == 2 ^ (h + 1) - 1)
            t
        )
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
path entry h = pathRec top (assert (entry >= 0 && entry < 2 ^ h) entry) 0
  where
    pathRec :: (Tree -> Loc) -> Int -> Int -> (Tree -> Loc)
    pathRec acc n i =
      if i == h
        then
          acc
        else
          pathRec (dir n i h . acc) n (i + 1)
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

toNum :: Loc -> Int
toNum l = foldl' (\acc (c, i) -> acc + c * 2 ^ i) 0 (zip (toBits l) ([0 ..] :: [Int]))
  where
    toBits :: Loc -> [Int]
    toBits (_, Top) = []
    toBits loc@(_, R _ _) = 1 : toBits (up loc)
    toBits loc@(_, L _ _) = 0 : toBits (up loc)

postorder :: (t -> Loc -> t) -> t -> Loc -> t
postorder f acc loc@(Leaf _, _) = f acc loc
postorder f acc loc = f acc'' loc
  where
    acc' = postorder f acc (left loc)
    acc'' = postorder f acc' (right loc)

harvest :: [Loc] -> Loc -> [Loc]
harvest ns (Node {}, _) = ns
harvest ns n = n : ns

harvestLeft :: Loc -> [Loc]
harvestLeft = harvestLeftRec []
  where
    harvestLeftRec :: [Loc] -> Loc -> [Loc]
    harvestLeftRec ns (_, Top) = ns
    harvestLeftRec ns loc@(_, L _ _) = harvestLeftRec ns (up loc)
    harvestLeftRec ns loc@(_, R _ _) = harvestLeftRec (postorder harvest ns (left parent)) parent
      where
        parent = up loc

harvestRight :: Loc -> [Loc]
harvestRight = harvestRightRec []
  where
    harvestRightRec :: [Loc] -> Loc -> [Loc]
    harvestRightRec ns (_, Top) = ns
    harvestRightRec ns loc@(_, R _ _) = harvestRightRec ns (up loc)
    harvestRightRec ns loc@(_, L _ _) = harvestRightRec (postorder harvest ns (right parent)) parent
      where
        parent = up loc

--

type Set = Tree

empty :: Set -> Bool
empty (Node _ _ False) = True
empty (Leaf False) = True
empty _ = False

minElem :: Set -> Maybe Loc
minElem = minElemLoc . top
  where
    minElemLoc :: Loc -> Maybe Loc
    minElemLoc (Node _ _ False, _) = Nothing
    minElemLoc loc@(Node l _ True, _) | not (empty l) = minElemLoc (left loc)
    minElemLoc loc@(Node _ _ True, _) = minElemLoc (right loc)
    minElemLoc loc@(Leaf True, _) = Just loc
    minElemLoc _ = error "unexpected case"
