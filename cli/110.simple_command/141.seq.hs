
-----------------------------------------------------------------------------
-- | Simple command like `seq 10 18`
--
-- Usage:
--   $ runhaskell THIS.hs
--
-----------------------------------------------------------------------------

main :: IO ()
main = do
    mapM_ (putStrLn . show) [start .. end]

-- | Numbers
start, end :: Int
start = 10
end   = 18
