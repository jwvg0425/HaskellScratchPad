import Data.List

sierpinsky :: Integral a => a -> String
sierpinsky = intercalate "\n" . tri

tri :: Integral a => a -> [String]
tri 0 = ["L"]
tri n = prev ++ [x ++ replicate ((len prev) - (length x) + 1) ' ' ++ x|x<-prev]
  where prev = tri (n-1)
        len = length.last

main = do
    n <- getLine
    putStrLn $ sierpinsky (read n :: Int)
    main