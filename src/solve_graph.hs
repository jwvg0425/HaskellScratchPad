type Node = Char
type Arc  = (Node, Node)

solveGraph :: Node -> Node -> [Arc] -> Bool
solveGraph s e arcs = dfs e s arcs []

dfs :: Node -> Node -> [Arc] -> [Node] -> Bool
dfs e n arcs visited
  | e == n = True
  | otherwise = foldl check False routes
  where routes = [x | x@(a,b) <- arcs, a == n]
        check acc x@(a,b) = if b `elem` visit then acc
                            else acc || (dfs e b arcs visit)
          where visit = n:visited