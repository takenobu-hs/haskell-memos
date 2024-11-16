
{-

  Atree

  Reference: IFPH 2nd, Ch.6

-}


-- | Main type
data Atree a = Leaf a | Fork Int (Atree a) (Atree a) deriving Show


-- |
-- >>> fork td2 td1
-- Fork 2 (Fork 1 (Leaf 20) (Leaf 30)) (Leaf 10)
fork :: Atree a -> Atree a -> Atree a
fork xt yt = Fork (lsize xt) xt yt

-- |
-- >>> lsize td3
-- 3
lsize :: Atree a -> Int
lsize (Leaf _)       = 1
lsize (Fork n xt yt) = n + (lsize yt)


-- |
-- >>> mkAtree [1..4]
-- Fork 2 (Fork 1 (Leaf 1) (Leaf 2)) (Fork 1 (Leaf 3) (Leaf 4))
mkAtree :: [a] -> Atree a
mkAtree xs
  | (m == 0)  = Leaf (unwrap xs)
  | otherwise = fork (mkAtree ys) (mkAtree zs)
  where
    m = (length xs) `div` 2
    (ys, zs) = splitAt m xs

unwrap :: [a] -> a
unwrap [x] = x


-- |
-- >>> retrieve td3 1
-- 30
-- >>> retrieve  (mkAtree [0..15]) 8
-- 8
--
-- Note: retrieve xt k == (flatten xt) !! k
--
retrieve :: Atree a -> Int -> a
retrieve (Leaf x) 0 = x
retrieve (Fork m xt yt) k
  | k < m     = retrieve xt k
  | otherwise = retrieve yt (k - m)




-- | Test data
-- 
td1 = Leaf 10
td2 = Fork 1 (Leaf 20) (Leaf 30)
td3 = Fork 1 (Leaf 20) (Fork 1 (Leaf 30) (Leaf 40))

