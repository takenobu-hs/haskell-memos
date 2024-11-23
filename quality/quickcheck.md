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

ghci> sample' ((arbitrary :: Gen Int) `suchThat` (>0))
[1,2,2,1,5,7,2,13,15,4,6]

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

ghci> replicateM 10 $ generate (arbitrary :: Gen Int)
[-29,5,-7,-12,-20,-2,-23,-2,-26,-12]

ghci> xs <- replicateM 10 $ generate (arbitrary :: Gen Int)
ghci> xs
[6,-21,-14,-14,-3,-28,-27,-6,23,-24]
```


#### judgement

```
ghci> quickCheck  (\x y -> x+y == y+x)
+++ OK, passed 100 tests.

ghci> verboseCheck (\x y -> x+y == y+x)
Passed:
0
0
:
```


### QuickCheck examples

#### arbitrary with type variable

```
 instance Arbitrary a => Arbitrary (Btree a) where
  arbitrary = oneof [ liftM  Leaf arbitrary
                    , liftM2 Fork arbitrary arbitrary]
```

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

### quickcheck in docteset

```
-- |
-- prop> add2 x y == add2 y x
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

### Misc

```
import Test.QuickCheck
stdArgs = Args
  { replay          = Nothing
  , maxSuccess      = 100
  , maxDiscardRatio = 10
  , maxSize         = 100
  , chatty          = True
quickCheck :: Testable prop => prop -> IO ()
quickCheck p = quickCheckWith stdArgs p

args = stdArgs { maxSuccess = 200 }
runq p = quickCheckWith args p
ghci> runq prop_plus1
+++ OK, passed 200 tests.
```


## References

* QuickCheck
  * https://hackage.haskell.org/package/QuickCheck
* Doctest
  * https://hackage.haskell.org/package/doctest

