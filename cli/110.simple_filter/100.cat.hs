
-----------------------------------------------------------------------------
-- | Simple IO like cat command
--
-- Usage:
--   $ runhaskell THIS.hs < INFILE
--
-----------------------------------------------------------------------------

main :: IO ()
main = do
    xs <- getContents
    putStr xs
