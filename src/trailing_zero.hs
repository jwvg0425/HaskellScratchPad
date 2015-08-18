import Data.List

zeros :: Integer -> Integer
zeros n = d + zeros d
    where d = n `div` 5