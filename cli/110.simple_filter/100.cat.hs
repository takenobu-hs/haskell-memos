
-----------------------------------------------------------------------------
-- | Simple command like `cat`
--
-- Usage:
--   $ runhaskell THIS.hs < INFILE
--
-----------------------------------------------------------------------------

main :: IO ()
main = do
    xs <- getContents
    putStr xs
