# Basic tools and environments

## Doctest with quickcheck

### Setup

Install dependencies for doctest.

```
$ cabal install QuickCheck --lib
$ cabal install template-haskell --lib
```

Install doctest.

```
$ cabal install doctest
```

### Example

Example check with doctest.

```
$ doctest Program.hs
```

### References

* Doctest
  * https://hackage.haskell.org/package/doctest
* QuickCheck
  * https://hackage.haskell.org/package/QuickCheck


## Haddock

### Setup

Install haddock.

```
$ cabal install haddock
```

### Example

```
$ haddock --html --hyperlinked-source -o dist Program.hs
```

### References

* Haddock
  * https://hackage.haskell.org/package/haddock
  * https://haskell-haddock.readthedocs.io/latest/
