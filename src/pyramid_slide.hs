longestSlideDown :: [[Int]] -> Int
longestSlideDown list = l (reverse list)
  where l (x:[]) = head x
        l (x:y:o) = l (s:o)
          where s = [k + max a b | n <- [0..(length y)-1], let k = y !! n, let [a,b] = take 2 $ drop n x]