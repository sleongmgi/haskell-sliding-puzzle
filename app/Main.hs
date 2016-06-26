module Main where

import qualified Data.ByteString.Char8 as C8
import System.IO
import qualified Data.List as L
import Data.Maybe
import System.Random
import Board
import Game

main :: IO ()
main = do
        newGame

newGame :: IO ()
newGame = do
        putStr "Define your board dimension (Format is XxY, exmple, 2x2 ): "
        hFlush stdout
        input <- getLine
        let (x, y) = dimension2D $ toList input
        g <- newStdGen
        let r = take 10 $ (randoms g :: [Direction])
        let answer = board (x, y)
        let game = shuffle ([N, W, N] ++ r) answer
        putStrLn $ "Board:\n" ++ show game
        play answer game

play :: Board -> Board -> IO ()
play answer game
  | answer == game = do
                        putStrLn "You win"
                        putStr "Would you like to play again [Y/N]: "
                        hFlush stdout
                        userMove <- getLine
                        case userMove of
                            "Y" -> newGame
                            "N" -> return ()
  | otherwise = do
        putStr "Enter your move [N|S|W|E] or Q to exit: "
        hFlush stdout
        userMove <- getLine
        case userMove of
           "Q" -> return ()
           a -> do
                  let newGame = move (read a :: Direction) game
                  putStrLn (show newGame)
                  play answer newGame

instance Random Direction where
  randomR (a, b) g =
      case randomR (fromEnum a, fromEnum b) g of
            (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

toList :: String -> [Int]
toList = (map read) . (map C8.unpack) . (C8.split 'x') . C8.pack

dimension2D :: [Int] -> (Int, Int)
dimension2D [] = error "No dimension values."
dimension2D [x] = error "Must be 2 numbers."
dimension2D [x, y] = (x, y)
dimension2D _ = error "Invalid dimension"

