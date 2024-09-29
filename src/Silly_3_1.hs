module Silly_3_1
  ( Tree (..),
    height,
    getMark,
    marked,
    --
    Ctx (..),
    Loc,
    left,
    right,
    top,
    up,
    upmost,
    --
    Order (..),
    harvest,
    harvestLeft,
    harvestRight,
    insert,
    delete,
    leaves,
    make,
    mark,
    modify,
    path,
    postorder,
    toNum,
    unmark,
    visit,
    --
    Set,
    setEmpty, -- S = ∅
    setDelete, -- S \ {j}
    setInsert, -- S ∪ {i}
    setMax, -- Compute the largest element of S
    setMin, -- Compute the least element of S
    setPred, -- Compute the largest element of S < j
    setSucc, -- Compute the least element of S > j
    setExtractMin, -- Delete the least element from S
    setExtractMax, -- Delete the largest element from S
  )
where

import Control.Exception (assert)
import Data.Bits

data Tree = Leaf Bool | Node Tree Tree Bool
  deriving (Show, Eq)

height :: Tree -> Int
height (Leaf _) = 0
height (Node l _ _) = height l + 1

marked :: Tree -> Bool
marked = getMark

getMark :: Tree -> Bool
getMark (Leaf m) = m
getMark (Node _ _ m) = m

-- Huet zipper. See
-- http://www.st.cs.uni-sb.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf
-- and https://wiki.haskell.org/Zipper
data Ctx = Top | L Ctx Tree | R Tree Ctx
  deriving (Show, Eq)

type Loc = (Tree, Ctx)

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

modify :: Loc -> (Tree -> Tree) -> Loc
modify (t, c) f = (f t, c)

mark :: Loc -> Loc
mark (Leaf _, c) = (Leaf True, c)
mark (Node l r _, c) = (Node l r True, c)

unmark :: Loc -> Loc
unmark (Leaf _, c) = (Leaf False, c)
unmark (Node l r _, c) = (Node l r False, c)

leaves :: Tree -> [Loc]
leaves t = reverse $ postorder f [] (top t)
  where
    f acc n = case n of (Node {}, _) -> acc; (Leaf {}, _) -> n : acc

insert :: Int -> Tree -> Int -> Tree
insert h t x =
  fst . upmost . mark $
    path
      (assert (x >= 1 && x <= 2 ^ h) x)
      (assert (h >= 0) h)
      t

delete :: Int -> Tree -> Int -> Tree
delete h t x =
  fst . upmost . unmark $
    path
      (assert (x >= 1 && x <= 2 ^ h) x)
      (assert (h >= 0) h)
      t

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
path entry h = pathRec top (assert (entry >= 1 && entry <= 2 ^ h) (entry - 1)) 0
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
toNum l@(Leaf _, _) = 1 + foldl' (\acc (c, i) -> acc + c * 2 ^ i) 0 (zip (toBits l) ([0 ..] :: [Int]))
  where
    toBits :: Loc -> [Int]
    toBits (_, Top) = []
    toBits loc@(_, R _ _) = 1 : toBits (up loc)
    toBits loc@(_, L _ _) = 0 : toBits (up loc)
toNum _ = error "toNum called on non-leaf"

--

data Order = Pre | In | Post

visit :: Order -> (a -> Loc -> a) -> a -> Loc -> a
visit Post = postorder
visit Pre = preorder
visit In = inorder

preorder :: (t -> Loc -> t) -> t -> Loc -> t
preorder f acc loc@(Leaf _, _) = f acc loc
preorder f acc loc =
  let acc' = f acc loc
      acc'' = preorder f acc' (left loc)
      acc''' = preorder f acc'' (right loc)
   in acc'''

inorder :: (t -> Loc -> t) -> t -> Loc -> t
inorder f acc loc@(Leaf _, _) = f acc loc
inorder f acc loc =
  let acc' = preorder f acc (left loc)
      acc'' = f acc' loc
      acc''' = preorder f acc'' (right loc)
   in acc'''

postorder :: (t -> Loc -> t) -> t -> Loc -> t
postorder f acc loc@(Leaf _, _) = f acc loc
postorder f acc loc =
  let acc' = postorder f acc (left loc)
      acc'' = postorder f acc' (right loc)
      acc''' = f acc'' loc
   in acc'''

--

harvest :: [Loc] -> Loc -> [Loc]
harvest ns (Node {}, _) = ns
harvest ns (Leaf False, _) = ns
harvest ns n@(Leaf True, _) = n : ns

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

minLoc :: Loc -> Maybe Loc
minLoc (Node _ _ False, _) = Nothing
minLoc loc@(Node l _ True, _) | not (setEmpty l) = minLoc (left loc)
minLoc loc@(Node _ _ True, _) = minLoc (right loc)
minLoc loc@(Leaf True, _) = Just loc
minLoc (Leaf False, _) = Nothing

maxLoc :: Loc -> Maybe Loc
maxLoc (Node _ _ False, _) = Nothing
maxLoc loc@(Node _ r True, _) | not (setEmpty r) = maxLoc (right loc)
maxLoc loc@(Node _ _ True, _) = maxLoc (left loc)
maxLoc loc@(Leaf True, _) = Just loc
maxLoc (Leaf False, _) = Nothing

--

type Set = Tree

setEmpty :: Set -> Bool
setEmpty (Node _ _ False) = True
setEmpty (Leaf False) = True
setEmpty _ = False

setMin :: Set -> Maybe Loc
setMin = minLoc . top

setMax :: Set -> Maybe Loc
setMax = maxLoc . top

setPred :: Loc -> Maybe Loc
setPred = setPredLoc
  where
    setPredLoc :: Loc -> Maybe Loc
    setPredLoc (_, Top) = Nothing
    setPredLoc loc@(_, R _ _) =
      case maxLoc (left (up loc)) of
        r@(Just _) -> r
        Nothing -> setPredLoc (up loc)
    setPredLoc loc@(_, L _ _) = setPredLoc (up loc)

setSucc :: Loc -> Maybe Loc
setSucc = setSuccLoc
  where
    setSuccLoc :: Loc -> Maybe Loc
    setSuccLoc (_, Top) = Nothing
    setSuccLoc loc@(_, L _ _) =
      case minLoc (right (up loc)) of
        r@(Just _) -> r
        Nothing -> setSuccLoc (up loc)
    setSuccLoc loc@(_, R _ _) = setSuccLoc (up loc)

setInsert :: Int -> Set -> Int -> Set
setInsert = insert

setDelete :: Int -> Set -> Int -> Set
setDelete = delete

setExtractMin :: Set -> Set
setExtractMin t = maybe t (fst . upmost . unmark) (setMin t)

setExtractMax :: Set -> Set
setExtractMax t = maybe t (fst . upmost . unmark) (setMax t)
