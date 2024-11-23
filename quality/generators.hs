
module Example where

import Test.QuickCheck
import Control.Monad

-- |
generatePair :: Gen (Int, Int)
generatePair = do
    x1 <- choose (1, 10)
    x2 <- choose (100, 300)
    return (x1, x2)

-- |
generateNList :: Gen [Int]
generateNList = do
    n  <- choose (1, 5)
    vector n
--  vectorOf n arbitrary


-- |
generateChars :: Gen [Char]
generateChars = do
    c1  <- choose ('a', 'e')
    c2  <- choose ('1', '7')
    return [c1, '-', c2]

