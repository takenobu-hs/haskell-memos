
-----------------------------------------------------------------------------
-- | Simple IO like cat command
--
-- Usage:
--   $ runhaskell THIS.hs < INFILE
--
-----------------------------------------------------------------------------

-- `interact` function is composed of getContents and putStr.
main :: IO ()
main = interact echo


-- | Echo a single line
-- >>> echo "abc"
-- "abc"
--
echo :: String -> String
echo s = s
