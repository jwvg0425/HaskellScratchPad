factorial n = product [1..n]
factorial' n = if n == 0 then 1 else n * factorial' (n-1)

fibo 0 = 1
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

lastButOne xs = last (init xs)

uppercase :: Char -> Char
uppercase ch | ch == 'a' = 'A' | ch == 'b' = 'B' | ch == 'c' = 'C' | ch == 'd' = 'D'
			 | ch == 'e' = 'E' | ch == 'f' = 'F' | ch == 'g' = 'G' | ch == 'h' = 'H'
			 | ch == 'i' = 'I' | ch == 'j' = 'J' | ch == 'k' = 'K' | ch == 'l' = 'L'
			 | ch == 'm' = 'M' | ch == 'n' = 'N' | ch == 'o' = 'O' | ch == 'p' = 'P'
			 | ch == 'q' = 'Q' | ch == 'r' = 'R' | ch == 's' = 'S' | ch == 't' = 'T'
			 | ch == 'u' = 'U' | ch == 'v' = 'V' | ch == 'w' = 'W' | ch == 'x' = 'X'
			 | ch == 'y' = 'Y' | ch == 'z' = 'Z' | otherwise = ch

uppercaseString :: String -> String
uppercaseString str = [uppercase ch | ch <- str]

changeFromList :: Char -> [(Char,Char)] -> Char
changeFromList ch [x] = if fst x == ch then snd x else ch
changeFromList ch str = if fst (head str) == ch then snd (head str) else changeFromList ch (tail str)
uppercase' :: Char -> Char
uppercase' ch = changeFromList ch alphabetPair
			where alphabetPair = zip ['a'..'z'] ['A'..'Z']
uppercaseString' str = [uppercase' ch | ch <-str]
uppercaseStringByMap str = map uppercase' str


fiboList :: Int -> [Int]
fiboList 0 = [1]
fiboList 1 = [1,1]
fiboList n = prevList ++ [lastButOne prevList + last prevList]
		where prevList = fiboList (n-1)
fibo' :: Int -> Int
fibo' n = last (fiboList n)

validParentheses :: String -> Bool
validParentheses = check 0

check :: Int -> String -> Bool
check 0 [] = True
check _ [] = False
check n (f:l) = if f == '(' then check (n+1) l else if n > 0 then check (n-1) l else False

fizzbuzz :: Int -> [Int]
fizzbuzz n = [three - fifth, five - fifth, fifth]
  where three = (n-1) `div` 3
        five = (n-1) `div` 5
        fifth = (n-1) `div` 15