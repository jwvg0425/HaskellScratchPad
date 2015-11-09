

data State = A | B | C | D deriving Eq
data 
type Action = Char

not11start :: State
not11start = A

not11end :: [State]
not11end = [A,B]

not11t A '0' = A
not11t A '1' = B
not11t B '0' = A
not11t B '1' = C
not11t C '0' = C
not11t C '1' = C

not11 = accept not11start not11end not11t

accept start end transition acts = ok $ foldl transition start acts
    where ok s = s `elem` end

end00start = A
end00end = [C]

end00t A '0' = B
end00t A '1' = A
end00t B '0' = C
end00t B '1' = A
end00t C '0' = C
end00t C '1' = A

end00 = accept end00start end00end end00t

have000start = A
have000end = [D]

have000t A '0' = B
have000t A '1' = A
have000t B '0' = C
have000t B '1' = A
have000t C '0' = D
have000t C '1' = A
have000t D _ = D

have000 = accept have000start have000end have000t