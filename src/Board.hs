module Board where
import Text.Printf

data Board = Board [[Int]] deriving (Eq)

class Elem a where
   elemAt :: (Int, Int) -> a -> Int
   find0 :: a -> (Int, Int)
   replace :: (Int, Int) -> Int -> a -> a
   swap :: (Int, Int) -> (Int, Int) -> a -> a


instance Show Board where
   show (Board board) = (join "\n" . map (\x -> (join " " . map (printf ("%" ++ show (maxElementLength (Board board)) ++ "d"))) x)) board

instance Elem Board where
   elemAt (x, y) (Board board) = (board!!y) !! x
   find0 (Board board) =
     head [(x, y) | (y, row) <- enumerate board,
                    (x, el)  <- enumerate row,
                    el == 0]
   replace (x, y) e (Board board) = Board (replaceAt y (replaceAt x e (board !! y)) board)
   swap c1 c2 b = (((replace c1 (elemAt c2 b)) . (replace c2 (elemAt c1 b))) b)

join :: Foldable t => [Char] -> t [Char] -> [Char]
join string list = foldr (\a b -> a ++ string ++ b) "" list

board :: (Int,Int) -> Board
board (x, y) = Board (splitEvery x $ [1..(x*y - 1)] ++ [0])

size :: Board -> Int
size (Board board) = length board * length (board !! 0)

maxElementLength :: Board -> Int
maxElementLength b = length (show ((size b) - 1))

enumerate :: [b] -> [(Int, b)]
enumerate = zip [0..]

replaceAt :: Int -> a -> [a] -> [a]
replaceAt index value list = take index list ++ [value] ++ drop (index + 1) list

-- splitEvery n = takeWhile (not . null) . unfoldr (Just . splitAt n)
splitEvery :: Int -> [t] -> [[t]]
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first, rest) = splitAt n list
