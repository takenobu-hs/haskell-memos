
-----------------------------------------------------------------------------
-- | Simple IO like cut -c1-20 command
--
-- Usage:
--   $ runhaskell THIS.hs < INFILE
--
-----------------------------------------------------------------------------

main :: IO ()
main = do
    xs <- getContents
    let xs2 = unlines . map (take nth) . lines $ xs
    putStr xs2

-- | Number of characters to cut
nth :: Int
nth = 30
