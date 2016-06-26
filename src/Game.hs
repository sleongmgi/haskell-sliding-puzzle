module Game where
import Board

data Direction = N | S | W | E deriving (Eq, Ord, Show, Read, Bounded, Enum)

shuffle :: [Direction] ->  Board -> Board
shuffle [] board = board
shuffle d board = ((shuffle (drop 1 d)) . (move (head d))) board

move :: Direction -> Board -> Board
move m b = case m of
                N -> move' (find0 b) (0, -1) b
                S -> move' (find0 b) (0, 1) b
                W -> move' (find0 b) (-1, 0) b
                E -> move' (find0 b) (1, 0) b

move' :: (Int, Int) -> (Int, Int) -> Board -> Board
move' (x, y) (x', y') (Board b)
  | x + x' >= length (b!!0) = (Board b)
  | x + x' < 0 = (Board b)
  | y + y' >= length b =(Board b)
  | y + y' < 0 = (Board b)
  | otherwise = swap (x, y) (x + x', y + y') (Board b)

