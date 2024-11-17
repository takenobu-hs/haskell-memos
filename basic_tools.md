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

### Eample

Example check with doctest.

```
$ doctest Program.hs
```


## Haddock

### Setup

Install haddock.

### Eample

```
$ haddock --html --hyperlinked-source -o dist Program.hs
```
