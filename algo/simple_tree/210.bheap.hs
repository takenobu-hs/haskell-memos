
{-

  Binary heap tree

  Reference: IFPH 2nd, Ch.6

-}

module Htrees where


-- | Main type
data Htree a = Null | Fork a (Htree a) (Htree a) deriving Show


-- |
-- >>> flatten td2
-- [1,10,20]
flatten :: Ord a => Htree a -> [a]
flatten Null           = []
flatten (Fork x xt yt) = x : merge (flatten xt) (flatten yt)

-- |
-- >>> merge [1,3 .. 9] [2,4 .. 10]
-- [1,2,3,4,5,6,7,8,9,10]
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys     = ys
merge (x:xs) [] = x:xs
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | otherwise = y: merge (x:xs) ys


-- |
-- >>> heapOrderd td2
-- True
-- >>> heapOrderd td2b
-- False
heapOrderd :: Ord a => Htree a -> Bool
heapOrderd = ordered . flatten

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
{-
mkHeap :: Ord a => [a] -> Htree a
mkHeap = heapify . mkHtree
-}


heapify :: Ord a => Htree a -> Htree a
heapify Null = Null
heapify (Fork x xt yt) = sift x (heapify xt) (heapify yt)

-- |
-- >>> sift 1000 td1b td1c
-- Fork 200 (Fork 1000 Null Null) (Fork 300 Null Null)
sift :: Ord a => a -> Htree a -> Htree a -> Htree a
sift x Null Null = Fork x Null Null
sift x (Fork y a b) Null
  | x <= y    = Fork x (Fork y a b) Null
  | otherwise = Fork y (sift x a b ) Null
sift x Null (Fork z c d)
  | x <= z    = Fork x Null (Fork z c d)
  | otherwise = Fork z Null (sift x c d)
sift x (Fork y a b) (Fork z c d)
  | x <= (y `min` z) = Fork x (Fork y a b) (Fork z c d)
  | y <= (x `min` z) = Fork y (sift x a b) (Fork z c d)
  | z <= (x `min` y) = Fork z (Fork y a b) (sift x c d)



-- |
-- >>> mkHtree [1..4]
-- Fork 1 (Fork 2 (Fork 4 Null Null) Null) (Fork 3 Null Null)
mkHtree :: Ord a => [a] -> Htree a
mkHtree = head . mkHtrees . levels


-- |
-- >>> levels [1..16]
-- [[1],[2,3],[4,5,6,7],[8,9,10,11,12,13,14,15],[16]]
levels :: [a] -> [[a]]
levels = levelsWith 1

-- |
-- >>> levelsWith 1 [1..16]
-- [[1],[2,3],[4,5,6,7],[8,9,10,11,12,13,14,15],[16]]
levelsWith :: Int -> [a] -> [[a]]
levelsWith _ [] = []
levelsWith n xs = ys : (levelsWith (n*2) zs)
  where
    (ys, zs) = splitAt n xs

-- |
-- >>> addLayer [1..3] [Null]
-- [Fork 1 Null Null,Fork 2 Null Null,Fork 3 Null Null]
--
-- e.g.: addLayer [1] (addLayer [2,3] (addLayer [4,5,6,7] ([Null])))
--
addLayer :: [a] -> [Htree a] -> [Htree a]
addLayer xs yt = loop xs (yt ++ repeat Null)
  where
    loop [] _ = []
    loop (x:xs) (y1:y2:ys) = (Fork x y1 y2) : loop xs ys

-- |
-- >>> mkHtrees [[1],[2,3]]
-- [Fork 1 (Fork 2 Null Null) (Fork 3 Null Null)]
mkHtrees :: Ord a => [[a]] -> [Htree a]
mkHtrees = foldr addLayer [Null]




-- | Test data
--
td1  = Fork 1 Null Null
td1b = Fork 200 Null Null
td1c = Fork 300 Null Null
td2  = Fork 1 (Fork 10 Null Null) (Fork 20 Null Null)
td2b = Fork 100 (Fork 10 Null Null) (Fork 20 Null Null)
td3  = [Fork n Null Null | n <- [1000 .. 1005]]



