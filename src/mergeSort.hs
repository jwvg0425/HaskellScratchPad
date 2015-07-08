merge xs [] = xs
merge [] ys = ys
merge xs@(x:xs') ys@(y:ys') = if x<y then x: (merge xs' ys) else y: (merge xs ys') 
mergeSort [] _ _ = []
mergeSort xs begin end = if begin == end then [xs !! begin] else merge frontList tailList
 where mid = (begin+end) `div` 2
       frontList = mergeSort xs begin mid
       tailList = mergeSort xs (mid+1) end