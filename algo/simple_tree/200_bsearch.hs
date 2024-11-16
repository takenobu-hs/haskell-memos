
{-

  Binary search tree

  Reference: IFPH 2nd, Ch.6

-}


-- | Main type
data Stree a = Null | Fork (Stree a) a (Stree a) deriving Show


-- |
-- >>> flatten td2
-- [1,10,20]
--
flatten :: Ord a => Stree a -> [a]
flatten Null           = []
flatten (Fork xt x yt) = flatten xt ++ [x] ++ flatten yt


-- |
-- >>> ordered [1,2,3]
-- True
-- >>> ordered [1,2,0]
-- False
ordered :: Ord a => [a] -> Bool
ordered []  = True
ordered [x] = True
ordered (x:y:zs)
  | x > y     = False
  | otherwise = ordered (y:zs)

-- |
-- >>> inordered td2
-- True
-- >>> inordered td2b
-- False
inordered :: Ord a => Stree a -> Bool
inordered = ordered . flatten


-- |
-- >>> member 10 td2
-- True
-- >>> member 11 td2
-- False
member :: Ord a => a -> Stree a -> Bool
member x Null = False
member x (Fork xt y yt) 
  | x == y    = True
  | x < y     = member x xt
  | otherwise = member x yt


-- |
-- >>> height td2
-- 2
height :: Ord a => Stree a -> Integer
height Null = 0
height (Fork xt x yt) = 1 + (height xt `max` height yt)


-- |
-- >>> mkStree [1,2,3]
-- Fork Null 1 (Fork Null 2 (Fork Null 3 Null))
mkStree :: Ord a => [a] -> Stree a
mkStree [] = Null
mkStree (x:xs) = Fork (mkStree ys) x (mkStree zs)
  where
    (ys, zs) = partition (<= x) xs

-- |
-- >>> partition (< 0) [-1,-3,0,2,3]
-- ([-1,-3],[0,2,3])
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = (filter p xs, filter (not . p) xs)

-- |
-- >>> sort [2,3,1]
-- [1,2,3]
--
-- prop> ordered $ sort xs 
-- True
sort :: Ord a => [a] -> [a]
sort = flatten . mkStree


-- |
-- >>> insert 10 $ insert 2 $ insert 5 Null
-- Fork (Fork Null 2 Null) 5 (Fork Null 10 Null)
insert :: Ord a => a -> Stree a -> Stree a
insert x Null = Fork Null x Null
insert x (Fork xt y yt)
  | x < y     = Fork (insert x xt) y yt
  | x == y    = Fork xt y yt
  | otherwise = Fork xt y (insert x yt)

-- |
-- >>> delete 2 $ mkStree [1..5]
-- Fork Null 1 (Fork Null 3 (Fork Null 4 (Fork Null 5 Null)))
-- >>> delete 4 $ mkStree [1..5]
-- Fork Null 1 (Fork Null 2 (Fork Null 3 (Fork Null 5 Null)))
delete :: Ord a => a -> Stree a -> Stree a
delete x Null = Null
delete x (Fork xt y yt)
  | x < y     = Fork (delete x xt) y yt
  | x == y    = join xt yt
  | otherwise = Fork xt y (delete x yt)

-- |
-- >>> join (mkStree [1,2]) (mkStree [3,4])
-- Fork (Fork Null 1 (Fork Null 2 Null)) 3 (Fork Null 4 Null)
--
-- Note: flatten (join xt yt) == flatten xt ++ flatten yt
join :: Ord a => Stree a -> Stree a -> Stree a
join xt yt
  | empty yt  = xt
  | otherwise = Fork xt (headTree) (tailTree)
  where
    (headTree, tailTree) = splitTree yt

empty:: Stree a -> Bool
empty Null         = True
empty (Fork _ _ _) = False

-- |
-- >>> splitTree (mkStree [1,2,3])
-- (1,Fork Null 2 (Fork Null 3 Null))
splitTree (Fork xt y yt)
  | empty xt  = (y, yt)
  | otherwise = (x, Fork wt y yt)
  where (x, wt) = splitTree xt




-- | Test data
--
td1  = Fork Null 1 Null
td2  = Fork (Fork Null 1 Null) 10 (Fork Null 20 Null)
td2b = Fork (Fork Null 30 Null) 10 (Fork Null 20 Null)
