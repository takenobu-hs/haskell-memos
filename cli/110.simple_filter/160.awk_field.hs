
-----------------------------------------------------------------------------
-- | Simple command like `awk '{print $3}'`
--
-- Usage:
--   $ runhaskell THIS.hs < INFILE
--
-----------------------------------------------------------------------------

main :: IO ()
main = do
    xs <- getContents
    let xs2 = unlines . map (getField nth) . lines $ xs
    putStr xs2

-- | Number of field to retrieve
nth :: Int
nth = 3

-- | Get Nth field
-- >>> getField 2 "aa bbb cc ddd"
-- "bbb"
getField :: Int -> String -> String
getField n s = if len >= n then ss!!(n-1) else ""
  where
    ss  = words s
    len = length ss
