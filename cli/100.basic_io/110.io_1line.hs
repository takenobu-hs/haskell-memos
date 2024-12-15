
-----------------------------------------------------------------------------
-- | Simple input and output a line
--
-- Usage:
--   $ runhaskell THIS.hs
--   ENTER a LINE
--
-----------------------------------------------------------------------------

main :: IO ()
main = do
    x <- getLine
    putStrLn x
