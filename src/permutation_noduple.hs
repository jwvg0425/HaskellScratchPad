import Data.List (nub, sort)

permutations :: String -> [String]
permutations str = permutation (sort str)

permutation [] = [""]
permutation [a] = [[a]]
permutation str = concat [map (h:) (permutation p) | (h, p) <- exceptList]
    where except idx = (take idx str) ++ (drop (idx+1) str)
          l = length str
          exceptList = nub $ sort [(str !! n, except n) | n <- [0..l-1]]