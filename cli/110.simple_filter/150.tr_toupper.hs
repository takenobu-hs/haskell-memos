
-----------------------------------------------------------------------------
-- | Simple IO like tr [:lower:] [:upper:]
--
-- Usage:
--   $ runhaskell THIS.hs < INFILE
--
-----------------------------------------------------------------------------

import Data.Char (toUpper)

main :: IO ()
main = do
    xs <- getContents
    let xs2 = map toUpper xs
    putStr xs2
