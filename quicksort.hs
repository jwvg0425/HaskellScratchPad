quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = smallerSorted ++ [x] ++ biggerSorted
  where smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x] 

-- 해당 원소가 있는 위치를 돌려줌
binarySearch :: (Ord a) => a -> [a] -> Int -> Int -> Int
binarySearch _ [] _ _ = -1
binarySearch x xs f l
		| f > l = (-1)
		| x == element = m
		| x > element = binarySearch x xs (m+1) l
		| x <= element = binarySearch x xs f (m-1)
  where m = (f + l) `div` 2
        element = xs !! m