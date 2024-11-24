
module Generators where

import Test.QuickCheck
import Control.Monad

--
-- ghci> sample' genPair
-- ghci> generate genPair
--
-- ghci> generate (arbitrary :: Gen (Btree Char))
--


----- basic types

-- |
genPair :: Gen (Int, Int)
genPair = do
    x1 <- choose (1, 10)
    x2 <- choose (100, 300)
    return (x1, x2)

-- |
genNList :: Gen [Int]
genNList = do
    n  <- choose (1, 5)
    vector n
--  vectorOf n arbitrary

-- |
genChars :: Gen [Char]
genChars = do
    c1  <- choose ('a', 'e')
    c2  <- choose ('1', '7')
    return [c1, '-', c2]

-- |
genMaybeInt :: Gen (Maybe Int)
genMaybeInt = frequency [
                (10, return Nothing)
              , (90, liftM Just arbitrary)
              ]



----- algebraic data types of enum

data Week = Sun | Mon | Tue | Wed | Thu | Fri | Sat
              deriving (Show, Enum, Bounded)

instance Arbitrary Week where
    arbitrary = chooseEnum (minBound, maxBound)

genWeek :: Gen Week
genWeek = arbitrary

genWeekday :: Gen Week
genWeekday = elements [Mon .. Fri]

genWeekend :: Gen Week
genWeekend = elements [Sun, Sat]

genWeekFreq :: Gen Week
genWeekFreq = frequency [
                (100, genWeekday)
              , ( 20, genWeekend)
              ]



----- algebraic data types of tree

data Btree a = Leaf a | Fork (Btree a) (Btree a) deriving Show

{- An infinite tree may be generated
instance Arbitrary a => Arbitrary (Btree a) where
    arbitrary = oneof [ liftM  Leaf arbitrary
                      , liftM2 Fork arbitrary arbitrary ]
-}

-- cf. "The fun of programming", Ch.2
instance Arbitrary a => Arbitrary (Btree a) where
    arbitrary = sized arbTree

arbTree :: Arbitrary a => Int -> Gen (Btree a)
arbTree 0         = liftM Leaf arbitrary
arbTree n | n > 0 = frequency [ (1, liftM  Leaf arbitrary)
                              , (3, liftM2 Fork shrub shrub) ]
  where shrub = arbTree (n `div` 2)

genBtreeInt :: Gen (Btree Int)
genBtreeInt = arbitrary

