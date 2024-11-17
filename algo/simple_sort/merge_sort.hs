
{-

  Merge sort

  Reference: IFPH 2nd, Ch.5

-}

module Msort where

-- |
-- >>> sortby id [2,9,0,1,5]
-- [0,1,2,5,9]
sortby f []       = []
sortby f [x]      = [x]
sortby f (x:y:xs) = mergeby f (cross (sortby f, sortby f) (divide (x:y:xs)))


-- |
-- >>> divide [1..11]
-- ([2,4,6,8,10],[1,3,5,7,9,11])
divide :: [a] -> ([a], [a])
divide = foldr allocate ([], [])
  where
    allocate x (ys, zs) = (zs, x:ys)

-- |
-- >>> mergeby id ([1,3 .. 9],[2,4 .. 10])
-- [1,2,3,4,5,6,7,8,9,10]
mergeby :: Ord b => (a -> b) -> ([a], [a]) -> [a]
mergeby f ([], ys)     = ys
mergeby f (x:xs, [])   = x:xs
mergeby f (x:xs, y:ys)
  | f x < f y = x : mergeby f (xs, y:ys)
  | otherwise = y : mergeby f (x:xs, ys)



pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)

cross :: (a -> b, c -> d) -> (a, c) -> (b, d)
cross (f, g) = pair (f . fst, g . snd)

