
-----------------------------------------------------------------------------
-- | Simple input and output lines
--
-- Usage:
--   $ runhaskell THIS.hs < INFILE
--
-----------------------------------------------------------------------------

main :: IO ()
main = do
    xs <- getContents
    putStr xs
