import Data.List

data Edge = Edge {label :: Char, from :: Int, to :: Int}

instance Show Edge where
    show e = show (label e, from e, to e)

data RE = Plus RE RE
        | Concat RE RE 
        | Closure RE 
        | Single Char
        | Epsilon 
        | Empty deriving Eq
    
isPlus :: RE -> Bool
isPlus (Plus _ _) = True
isPlus _ = False

isEpsilon :: RE -> Bool
isEpsilon Epsilon = True
isEpsilon _ = False

isConcat :: RE -> Bool
isConcat (Concat _ _) = True
isConcat _ = False

isClosure :: RE -> Bool
isClosure (Closure _) = True
isClosure _ = False

isSingle :: RE -> Bool
isSingle (Single _) = True
isSingle _ = False

left :: RE -> RE
left (Plus a b) = a
left (Concat a b) = a
left k = k

right :: RE -> RE
right (Plus a b) = b
right (Concat a b) = b
right k = k

instance Read RE where
    read [a] = Single a
    read 

instance Show RE where
    show (Plus a b) = show a ++ " + " ++ show b
    show (Concat a b) = sa ++ sb
        where sa = if isPlus a then "(" ++ show a ++ ")" else show a
              sb = if isPlus b then "(" ++ show b ++ ")" else show b
    show (Closure k)
        | isEpsilon k = show k ++ "*"
        | isSingle k = show k ++ "*"
        | otherwise = "(" ++ show k ++ ")*"
    show (Single k) = [k] 
    show Epsilon = "%"
    show Empty = ""

type Graph = [Edge]

makeGraph :: [(Char, Int, Int)] -> Graph
makeGraph edges = foldr (\(l,f,t) acc -> Edge {label = l, from = f, to = t}:acc) [] edges

reduce :: RE -> RE
reduce r = if go r == r then r else reduce (go r)
    where go (Plus Empty Empty) = Empty
          go (Plus a Empty) = go a
          go (Plus Empty b) = go b
          go (Plus a b) = Plus (go a) (go b)
          go (Concat Empty _) = Empty
          go (Concat _ Empty) = Empty
          go (Concat Epsilon Epsilon) = Epsilon
          go (Concat a Epsilon) = go a
          go (Concat Epsilon b) = go b
          go (Concat a b) = Concat (go a) (go b)
          go (Closure Empty) = Empty
          go (Closure Epsilon) = Epsilon
          go (Closure k) = Closure (go k)
          go (Single k) = Single k
          go Epsilon = Epsilon
          go Empty = Empty

removeEpsilon :: RE -> RE
removeEpsilon (Plus a b)
    | a == Epsilon = b
    | b == Epsilon = a
    | a == Epsilon && b == Epsilon = Empty
    | otherwise = (removeEpsilon a) `Plus` (removeEpsilon b)
removeEpsilon Epsilon = Empty
removeEpsilon k = k

-- a contain b
contain :: RE -> RE -> Bool
contain _ Empty = True
contain k@(Plus a b) r
    | isPlus a = partial || ((left a) `Plus` ((right a) `Plus` b)) `contain` r
    | otherwise = partial
        where partial = a `contain` r || b `contain` r || k == r
contain k@(Concat a b) Epsilon = (a `contain` Epsilon && b `contain` Epsilon)
contain k@(Concat a b) r = k == r || (a `contain` Epsilon && b `contain` (removeEpsilon r)) || (a `contain` (removeEpsilon r) && b `contain` Epsilon)
contain a@(Closure k) r = a ==r || k `contain` (removeEpsilon r) && r `contain` Epsilon
contain (Single k) (Single l) = k == l
contain (Single k) _ = False
contain Epsilon Epsilon = True
contain Epsilon _ = False
contain Empty _ = False

moreReduce :: RE -> RE
moreReduce r = if go r' == r' then r' else moreReduce (go r')
    where r' = reduce r
          go r@(Plus a b)
            | a `contain` b = a
            | b `contain` a = b
            | otherwise = (go a) `Plus` (go b)
          go r@(Concat a b)
            | a `contain` Epsilon && b `contain` a && isClosure b = b
            | b `contain` Epsilon && a `contain` b && isClosure a = a
            | otherwise = foldl (\x y -> (go x) `Concat` (go y)) (head rs) (tail rs)
                where rs = concatListReduce (concatToList r)
          go (Closure k)
            | isPlus k && k `contain` Epsilon = Closure (removeEpsilon k)
            | otherwise = Closure (go k)
          go k = k
          concatToList (Concat a b) = concatToList a ++ concatToList b
          concatToList k = [k]
          concatReduce r@(Concat a b)
            | a `contain` Epsilon && b `contain` a && isClosure b = b
            | b `contain` Epsilon && a `contain` b && isClosure a = a
            | otherwise = r
          concatListReduce [] = []
          concatListReduce [a] = [a]
          concatListReduce (a:b:l) = if rab == ab then a:concatListReduce (b:l) else rab:concatListReduce l
            where ab = a `Concat` b
                  rab = concatReduce ab

toRe :: Graph -> Int -> Int -> RE
toRe g start end = go start end (length g)
    where direct i j = filter (\x-> from x == i && to x == j) g
          go i j 0 = if i == j then Plus Epsilon s else s
            where s = foldl (\acc x -> Plus acc $ Single (label x)) Empty (direct i j)
          go i j k = Plus (go i j (k-1)) (Concat (Concat (go i k (k-1)) (Closure (go k k (k-1)))) (go k j (k-1)))