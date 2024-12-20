
-----------------------------------------------------------------------------
-- | Simple command like `sort`
--
-- Usage:
--   $ runhaskell THIS.hs < INFILE
--
-----------------------------------------------------------------------------

import Data.List (sort)

main :: IO ()
main = do
    xs <- getContents
    let xs2 = unlines . sort . lines $ xs
    putStr xs2
