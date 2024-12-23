# Quality

## Quickcheck

### Interactive examples

#### basic elements by `sample` and `sample'`

```
$ ghci
ghci> import Test.QuickCheck

ghci> sample $ elements [0,1,2,3,5,8]
3
1
8
0
1
0
2
2
3
0
2

ghci> sample' $ elements [0,1,2,3,5,8]
[8,8,0,2,1,8,0,2,0,2,3]
```


#### `elements`, `choose`, `oneof`, and `frequency`

```
ghci> sample' $ elements ["Jan", "Feb", "Mar"]
["Jan","Jan","Mar","Feb","Jan","Jan","Feb","Mar","Jan","Jan","Feb"]

ghci> sample' $ choose (1, 10)
[10,7,1,5,10,1,10,8,6,9,5]

ghci> sample' $ choose ('a','z')
"srwvrbfqstb"

ghci> sample' $ oneof [choose (100, 110), choose (200, 210), choose (300, 310)]
[207,110,107,110,305,302,206,200,306,110,203]

ghci> sample' $ frequency [(30, elements [1, 2, 3]), (70, elements [101, 102, 103])]
[2,101,103,101,101,103,1,102,1,101,102]
```


#### `arbitrary`

```
ghci> sample' (arbitrary :: Gen Int)
[0,1,4,-2,-4,10,4,2,2,13,20]

ghci> sample' (arbitrary :: Gen [Int])
[[],[],[-1,-2,-2],[0,3,-6],[8,0,2,-5,-4,8,-2,-7],[-4,-3,3,8,-10],[-3,-4,-9,6,4,7,12,7,1,-9,0,-3],[5,-9,-8,-13,12,10,-3],[-1,-8,-12,4,14,-5],[-18,-7,11,-4,18,-2,3,16,-10,10],[-10,-7,-9,19,7,14,-3,3,8,12,14,13,-13,-17,-11,-5]]
```


#### `resize`

```
ghci> sample' $ (arbitrary :: Gen Int)
[0,1,-3,-3,-3,9,4,-13,16,-2,6]

ghci> sample' $ resize 100000 $ (arbitrary :: Gen Int)
[-75191,21304,23337,71894,-62696,66980,90890,-86312,67257,-5797,55330]
```


#### `suchThat`

```
ghci> sample' ((arbitrary :: Gen Int) `suchThat` even)
[0,0,0,-6,4,2,10,-8,12,16,-2]

ghci> sample' ((arbitrary :: Gen Int) `suchThat` (> 0))
[1,2,2,1,5,7,2,13,15,4,6]

ghci> sample' (arbitrary `suchThat` (> 0))
[2,1,3,3,6,10,5,3,12,5,4]

ghci> sample' (arbitrary `suchThat` ( \(x,y) -> x > 0 && y > 0 ))
[(1,4),(4,2),(4,8),(1,4),(7,7),(6,6),(14,19),(16,7),(4,7),(6,2),(15,20)]
```


#### utilities

```
ghci> sample' (listOf (arbitrary:: Gen Int))
ghci> sample' (listOf1 (arbitrary:: Gen Int))
ghci> sample' (vectorOf 3 (arbitrary:: Gen Int))
ghci> sample' (shuffle [1..5])
ghci> sample' (vector 3 :: Gen [Int])
ghci> sample' (orderedList :: Gen [Int])
```


#### algebraic data types

```
ghci> data Signal = Green | Yellow | Red deriving (Show, Enum)
ghci> sample' $ elements [Green, Yellow, Red]
[Green,Yellow,Green,Yellow,Yellow,Red,Green,Yellow,Green,Green,Green]

ghci> sample' $ chooseEnum (Green, Red)
[Red,Green,Yellow,Yellow,Green,Yellow,Yellow,Yellow,Green,Green,Green]

ghci> instance Arbitrary Signal where arbitrary = chooseEnum (Green, Red)
ghci> sample' (arbitrary :: Gen Signal)
[Green,Yellow,Red,Yellow,Green,Yellow,Yellow,Red,Green,Green,Green]
```

```
ghci> data Btree a = Leaf a | Fork (Btree a) (Btree a) deriving Show
ghci> instance Arbitrary a => Arbitrary (Btree a) where arbitrary = oneof [ liftM Leaf arbitrary, liftM2 Fork arbitrary arbitrary]
ghci> sample' (arbitrary :: Gen (Btree Int))
```


#### `generate`

```
ghci> import Control.Monad
ghci> generate (arbitrary :: Gen Int)
2

ghci> generate $ vectorOf 10 (arbitrary :: Gen Int)
[23,-7,19,-8,-17,-1,-5,21,15,0]

ghci> xs <- generate $ vectorOf 10 (arbitrary :: Gen Int)
ghci> xs
[22,29,23,-20,1,7,-14,22,13,2]
```


#### judgement

```
ghci> quickCheck  (\x y -> x+y == y+x)
+++ OK, passed 100 tests.

ghci> quickCheck  ( (\x y -> x+y == y+x) :: (Int -> Int -> Bool))
+++ OK, passed 100 tests.

ghci> verboseCheck (\x y -> x+y == y+x)
Passed:
0
0
:
```

#### statistics

```
ghci> quickCheck $ (\x y -> collect (x, y) (x+y == y+x))
+++ OK, passed 100 tests:
 2% (-14,14)
 2% (0,0)
 1% (-1,-57)
 1% (-11,21)
 :
```

```
ghci> quickCheck $ (\xs -> classify (null xs) "empty lists" $ classify (length xs == 1) "unit lists" $ xs == xs)
+++ OK, passed 100 tests:
 5% empty lists
 2% unit lists

```



### QuickCheck examples

#### define a generator with do-expression

```
generatePair :: Gen (Int, Int)
generatePair = do
    x1 <- choose (1, 10)
    x2 <- choose (100, 300)
    return (x1, x2)
```

```
ghci> sample' generatePair
[(9,227),(8,272),(6,201),(1,227),(6,121),(1,211),(7,123),(6,113),(5,233),(10,107),(3,228)]
```

#### arbitrary with type variable

```
 instance Arbitrary a => Arbitrary (Btree a) where
  arbitrary = oneof [ liftM  Leaf arbitrary
                    , liftM2 Fork arbitrary arbitrary]
```

#### finite data with `sized`

```
data Btree a = Leaf a | Fork (Btree a) (Btree a) deriving Show


-- cf. "The fun of programming", Ch.2
instance Arbitrary a => Arbitrary (Btree a) where
    arbitrary = sized arbTree

arbTree :: Arbitrary a => Int -> Gen (Btree a)
arbTree 0         = liftM Leaf arbitrary
arbTree n | n > 0 = frequency [ (1, liftM  Leaf arbitrary)
                              , (3, liftM2 Fork shrub shrub) ]
  where shrub = arbTree (n `div` 2)
```



### quickcheck in docteset

```
-- prop> add2 x y == add2 y x
```

```
-- prop> (withMaxSize 1023 (\x y -> x + y == y + x))
-- prop> (forAll (resize 1023 arbitrary) (\(x, y) -> x + y == y + x))
-- prop> (forAll (choose (0,1023)) (\x -> x + 1 == 1 + x))
-- prop> (forAll (choose (0,10)) (\x -> (forAll (choose (-10, -1)) (\y -> (x + y == y + x)))))
```

```
-- | Prop test
-- $setup
-- >>> import Test.QuickCheck
-- >>> :{
-- instance Arbitrary Hex where
--   arbitrary = Hex `fmap`
--                 oneof [ choose (0x0000000000000000,0x000000000000ffff)
--                       , choose (0x0000000fffffff00,0x000000ffffffffff)
--                       , choose (0xffffffffffff0000,0xffffffffffffffff)
--                       , choose (0x0000000000000000,0xffffffffffffffff) ]
-- :}
```

```
-- $setup
-- >>> import Test.QuickCheck
-- >>> :{
-- imply cond prop = forAll (arbitrary `suchThat` cond ) prop
-- (|=>) cond prop = imply cond prop
-- >>> :}

-- prop> (\(x,y) -> (y /= 0)) |=> (\(x,y) -> (x `div` y) == ((x*2) `div` (y*2)))
```

```
-- | Prop test
-- $setup
-- >>> import Test.QuickCheck
-- >>> :{
-- genInt255 :: Gen Int
-- genInt255 = choose (0, 255)
-- :}

-- prop> forAll genInt255  (\x -> x < 256)
```


### Misc

```
ghci> quickCheckWith (stdArgs {maxSize = 10000}) (\x y -> x+y == y+x)
+++ OK, passed 100 tests.
```

```
stdArgs = Args
  { replay          = Nothing
  , maxSuccess      = 100
  , maxDiscardRatio = 10
  , maxSize         = 100
  , chatty          = True
  , maxShrinks      = maxBound
  }
```

```
runAll = mapM_ quickCheck [prop_plus1, prop_plus2, prop_plus3]
```


## References

* QuickCheck
  * https://hackage.haskell.org/package/QuickCheck
* Doctest
  * https://hackage.haskell.org/package/doctest

