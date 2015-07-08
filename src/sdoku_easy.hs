import Data.List

sudoku puzzle = if length (filter (==0) (concat puzzle)) == 0 
                then puzzle
                else sudoku [[if val x y == 0 then solve x y puzzle else val x y | x <- [0..8]] | y <-[0..8]]
                where val x y = (puzzle !! y) !! x

cellList n = [start..end]
  where cell = n `div` 3
        start = cell * 3
        end = (cell+1) * 3 - 1

solve x y puzzle = if length candidate == 1 then head candidate else 0
  where celld = filter (/=0) [val c r | c <- xcell, r <- ycell]
        xd = filter (/=0) [val c y | c <- [0..8]]
        yd = filter (/=0) [val x r | r <- [0..8]]
        duple = nub $ sort (celld ++ xd ++ yd)
        candidate = [1..9] \\ duple
        val x y = (puzzle !! y) !! x
        xcell = cellList x
        ycell = cellList y