
-----------------------------------------------------------------------------
-- | Simple command like `sed -e 's/ABC/XYZ/g`
--
-- Usage:
--   $ runhaskell THIS.hs < INFILE
--
-- Package requirements:
--   https://hackage.haskell.org/package/split for `splitOn`
--
-----------------------------------------------------------------------------

import Data.List (intercalate)
import Data.List.Split (splitOn)

main :: IO ()
main = do
    xs <- getContents
    let xs2 = unlines . map (substitute "ABC" "XYZ") . lines $ xs
    putStr xs2


-- | Substitute strings
-- >>> substitute "abc" "xyz" "abc123abc456abc789abc"
-- "xyz123xyz456xyz789xyz"
substitute :: String -> String -> String -> String
substitute pat1 pat2 s = intercalate pat2 $ splitOn pat1 s


