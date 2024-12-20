
-----------------------------------------------------------------------------
-- | Simple command like `cut -c1-30`
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
