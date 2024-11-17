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


## Haddock

### Setup

Install haddock.

### Example

```
$ haddock --html --hyperlinked-source -o dist Program.hs
```
