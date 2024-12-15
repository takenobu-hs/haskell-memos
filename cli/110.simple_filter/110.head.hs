
-----------------------------------------------------------------------------
-- | Simple IO like head command
--
-- Usage:
--   $ runhaskell THIS.hs < INFILE
--
-----------------------------------------------------------------------------

main :: IO ()
main = do
    xs <- getContents
    let xs2 = unlines . take nth . lines $ xs
    putStr xs2

-- | Number of lines to retrieve
nth :: Int
nth = 5
