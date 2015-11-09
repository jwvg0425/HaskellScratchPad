import Data.Numbers.Primes

triList = map tri [1..]
    where tri n = sum [1..n]
    
divisorNum n = length [x | x <- [1..n], n `mod` x == 0]

answer = head $ dropWhile (\x -> divisorNum x < 500) triList 