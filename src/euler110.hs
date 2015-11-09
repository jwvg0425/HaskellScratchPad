import Data.List
import Data.Numbers.Primes

num :: [Integer] -> Integer
num qs = product $ zipWith (^) primes qs

kind :: [Integer] -> Integer
kind qs = 1 + go qs 1
    where go [] _ = 0
          go (q:qs) k = q*k + go qs (k*(q*2+1))
    
primeSumLists k = tail $ go smallPrime 1 k
    where smallPrime = takeWhile (<k) primes
          go [] _ _ = [[]]
          go (p:ps) l k = [x:y | x <- [0..k], let s = l*(p^x), s < k, y <- go ps s k]

answer :: Integer -> Integer
answer k = k
    where cands = (inits . repeat) 1
          longestCand = head $ dropWhile (\x -> kind x < k) cands
          p = tail $ zipWith (*) primes longestCand