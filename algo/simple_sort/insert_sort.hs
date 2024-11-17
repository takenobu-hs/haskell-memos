
{-

  Insertion sort

  Reference: IFPH 2nd, Ch.5

-}


-- |
-- >>> sortby id [2,6,1,0,8]
-- [0,1,2,6,8]
sortby :: Ord b => (a -> b) -> [a] -> [a]
sortby f = foldr (insertby f) []


-- |
-- >>> insertby id 3 [0,2 .. 6]
-- [0,2,3,4,6]
insertby :: Ord b => (a -> b) -> a -> [a] -> [a]
insertby f x = insert . span test
  where
    insert (xs, ys) = xs ++ [x] ++ ys
    test y = (f y < f x)

