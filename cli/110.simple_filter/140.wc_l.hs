
-----------------------------------------------------------------------------
-- | Simple IO like wc -l command
--
-- Usage:
--   $ runhaskell THIS.hs < INFILE
--
-----------------------------------------------------------------------------

main :: IO ()
main = do
    xs <- getContents
    let xs2 = length . lines $ xs
    putStrLn $ show xs2
