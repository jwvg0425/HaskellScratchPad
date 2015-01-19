merge xs [] = xs
merge [] ys = ys
merge xs@(x:xs') ys@(y:ys') = if x<y then x: (merge xs' ys) else y: (merge xs ys') 
mergeSort [] _ _ = []
mergeSort xs first last = if first == last then [xs !! first] else merge frontList tailList
 where mid = (first+last) `div` 2
       frontList = mergeSort xs first mid
       tailList = mergeSort xs (mid+1) last