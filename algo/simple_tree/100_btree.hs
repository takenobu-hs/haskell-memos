
{-

  Binary tree

  Reference: IFPH Ch.6

-}


-- | Main type
data Btree a = Leaf a | Fork (Btree a) (Btree a) deriving Show



-- |
-- >>> size td3
-- 3
--
-- Note: size == length . flatten
--
-- prop> (size $ mkBtree xt) == (length . flatten $ mkBtree xt)
size :: Btree a -> Int
size (Leaf _)     = 1
size (Fork xt yt) = size xt + size yt


-- |
-- >>> flatten td3
-- [1,2,3]
flatten :: Btree a -> [a]
flatten (Leaf x)     = [x]
flatten (Fork xt yt) = flatten xt ++ flatten yt


-- |
-- >>> nodes td3
-- 2
--
-- Note: size == 1 + nodes
--
-- prop> (size $ mkBtree xt) == (1 + nodes (mkBtree xt))
--
nodes :: Btree a -> Int
nodes (Leaf _) = 0
nodes (Fork xt yt) = 1 + nodes xt + nodes yt


-- |
-- >>> depths td3
-- Fork (Leaf 1) (Fork (Leaf 2) (Leaf 2))
depths :: Btree a -> Btree Int
depths = down 0

down :: Int -> Btree a -> Btree Int
down n (Leaf _)     = Leaf n
down n (Fork xt yt) = Fork (down (n+1) xt) (down (n+1) yt)


-- |
-- >>> maxBtree td3
-- 3
--
-- Note: height == maxBtree . depths
--
maxBtree :: Ord a => Btree a -> a
maxBtree (Leaf x) = x
maxBtree (Fork xt yt) = maxBtree xt `max` maxBtree yt


-- |
-- >>> mkBtree [1..5]
-- Fork (Fork (Leaf 1) (Leaf 2)) (Fork (Leaf 3) (Fork (Leaf 4) (Leaf 5)))
mkBtree :: [a] -> Btree a
mkBtree xs
  | (m == 0)  = Leaf (unwrap xs)
  | otherwise = Fork (mkBtree ys) (mkBtree zs)
  where
    m = (length xs) `div` 2
    (ys, zs) = splitAt m xs

unwrap :: [a] -> a
unwrap [x] = x


-- |
-- >>> mapBtree (+1) td3
-- Fork (Leaf 2) (Fork (Leaf 3) (Leaf 4))
mapBtree :: (a -> b) -> Btree a -> Btree b
mapBtree f (Leaf x)     = Leaf (f x)
mapBtree f (Fork xt yt) = Fork (mapBtree f xt) (mapBtree f yt)


-- |
-- >>> foldBtree (*10) (+) td3
-- 60
--
-- Note:
--   size = foldBtree (const 1) (+)
--   height = foldBtree (const 0) f   where f m n = 1 + (m `max` n)
--   flatten = foldBtree wrap (++)
--   maxBtree = foldBtree id max
--   mapBtree = foldBtree (Leaf . f) Fork
foldBtree :: (a -> b) -> (b -> b -> b) -> Btree a -> b
foldBtree f g (Leaf x)     = f x
foldBtree f g (Fork xt yt) = g (foldBtree f g xt) (foldBtree f g yt)



-- | Test data
--
td1 = Leaf 1
td2 = Fork (Leaf 2) (Leaf 3)
td3 = Fork td1 td2

