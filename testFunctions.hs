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

alphabetPair = zip ['a'..'z'] ['A'..'Z']
changeFromList :: Char -> [(Char,Char)] -> Char
changeFromList ch [x] = if fst x == ch then snd x else ch
changeFromList ch str = if fst (head str) == ch then snd (head str) else changeFromList ch (tail str)
uppercase' :: Char -> Char
uppercase' ch = changeFromList ch alphabetPair
uppercaseString' str = [uppercase' ch | ch <-str]