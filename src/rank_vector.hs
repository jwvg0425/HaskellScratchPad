import Data.List

ranks :: (Eq a, Ord a) => [a] -> [Int]
ranks list = map rank list
  where rank n = succ . length $ filter (n<) list