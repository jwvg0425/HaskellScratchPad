explosiveSum :: Int -> Integer
explosiveSum n
  | n < 0 = 0
  | otherwise = s n n  

memo f = map (\x -> map (f x) [0..]) [0..]
store = memo s

s 0 _ = 1
s n k = sum [memos (n-l) l | l <- [1..r]]
    where r = if n > k then k else n

memos n k = store !! n !! k