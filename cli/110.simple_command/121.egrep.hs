
-----------------------------------------------------------------------------
-- | Simple command like `egrep`
--
-- Usage:
--   $ runhaskell THIS.hs < INFILE
--
-- Package requirements:
--   https://hackage.haskell.org/package/regex-posix for `=~`
--
-----------------------------------------------------------------------------

import Text.Regex.Posix ((=~))

main :: IO ()
main = do
    xs <- getContents
    let xs2 = unlines . filter isPattern . lines $ xs
    putStr xs2


-- | Pattern for egrep
--
-- >>> isPattern "Janualy"
-- True
--
-- >>> isPattern "Febrary"
-- False
--
isPattern :: String -> Bool
isPattern = (=~ "(Jan|Jul)")
