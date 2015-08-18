import Data.Char
import Data.Numbers.Primes

cand :: Int -> Int -> Int -> [Int]
cand n d k
    | n < k = []
    | n == 0 = [0]
    | k == 0 = notD
    | otherwise = notD ++ inD
    where notD = [prev*10 + now | prev <- cand (n-1) d k, now <- filter (/=d) [0..9]]
          inD = [prev*10 + d | prev <- cand (n-1) d (k-1)]
          
list :: Int -> Int -> Int -> [Int]
list n d k = filter (\x-> isPrime x && x >= 10^(n-1)) $ cand n d k          
                  
get :: Int -> Int -> [Int]
get n d = go n d n
    where go n d k = if length l == 0 then go n d (k-1) else l
            where l = list n d k
                
s :: Int -> Int
s n = (sum . concat) [get n d | d <- [0..9]]