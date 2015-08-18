import Control.Monad
import Data.List

factorial n = product [1..n]

powerset :: [a] -> [[a]]
powerset = filterM $ \x->[True, False]

combination :: [a] -> Int -> [[a]]
combination list k = filter (\x -> length x == k) $ powerset list
    
permutation :: Eq a => [a] -> [[a]]
permutation = nub . perm
    where perm [] = [[]]
          perm list = [ x:xs | x <- list, xs <- perm $ delete x list ]