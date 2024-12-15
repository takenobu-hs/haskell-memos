
-----------------------------------------------------------------------------
-- | Simple IO like egrep command
--
-- Usage:
--   $ runhaskell THIS.hs < INFILE
--
-----------------------------------------------------------------------------

import Text.Regex.Posix

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
