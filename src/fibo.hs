memo = map f [0..]

f 0 = 0
f 1 = 1
f n = memo !! (n-1) + memo !! (n-2)

k = sum $ filter ((==0).(`mod`2)) $ takeWhile (<= 4000000) memo