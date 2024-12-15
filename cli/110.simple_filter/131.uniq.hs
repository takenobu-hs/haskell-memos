
-----------------------------------------------------------------------------
-- | Simple IO like uniq command
--
-- Usage:
--   $ runhaskell THIS.hs < INFILE
--
-----------------------------------------------------------------------------

import Data.List (nub)

main :: IO ()
main = do
    xs <- getContents
    let xs2 = unlines . nub . lines $ xs
    putStr xs2
