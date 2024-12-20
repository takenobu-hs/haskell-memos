
-----------------------------------------------------------------------------
-- | Simple command like `fgrep`
--
-- Usage:
--   $ runhaskell THIS.hs < INFILE
--
-----------------------------------------------------------------------------

import Data.List (isInfixOf)

main :: IO ()
main = do
    xs <- getContents
    let xs2 = unlines . filter isPattern . lines $ xs
    putStr xs2


-- | Pattern for fgrep
--
-- >>> isPattern  "Feb Mar Apr May"
-- True
-- >>> isPattern  "Sep Oct Nov Dec"
-- False
--
isPattern :: String -> Bool
isPattern s = isInfixOf "Mar" s
