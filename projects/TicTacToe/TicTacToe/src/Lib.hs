{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( move,
      whoWon,
      playerAt,
      takeBack,
      isDraw
    ) where

import Data.List (tails)
import qualified Data.Vector as V
import Control.Lens
import Data.Vector.Lens

someFunc :: IO ()
someFunc = putStrLn "someFunc"

  -- The position is a pair (x, y)
type Position = (Int, Int)

-- The board is two lists of positions
data Board = Board {
  _x, _o :: [Position]
  } deriving (Eq, Show)

makeLenses ''Board

data Player = X | O deriving (Eq, Show)

data Cell = EmptyCell | Occupied Player deriving (Eq, Show)

-- type Board = [[Cell]]

emptyBoard :: [Position]
emptyBoard = [(x, y) | x <- [1..3], y <- [1..3]]

-- `move`: takes a tic-tac-toe board and position and moves to that
-- position (if not occupied) returning a new board. This function can
-- only be called on a board that is empty or in-play. Calling `move`
-- on a game board that is finished is a *compile-time type error*.

-- (optional) If fewer than 5 moves have been played, then this
-- guarantees that the game is still in play, and so calling `move`
-- will never produce a type-error in this case.

move ::
  Position
  -> Board
  -> Board

move p b
  | gameFinished b = error "Game finished"
  | p `elem` _x b || p `elem` _o b = error "Position occupied"
  | otherwise = over (playerMoves $ playerAt b) ((:) p) b


-- `whoWon`: takes a tic-tac-toe board and returns the player that
-- won the game (or a draw if neither). This function can only be
-- called on a board that is finished. Calling `whoWon` on a game
-- board that is empty or in-play is a *compile-time type error*. As
-- an optional consideration, `whoWon` should never be a draw if fewer
-- than nine moves have been played. In the case that the game is
-- completed, but fewer than nine moves have been played, return a
-- value that can only be one of two possibilities (the winner) and
-- never a draw.

whoWon ::
  Board
  -> Either String Player

whoWon = undefined

-- `playerAt`: takes a tic-tac-toe board and position and returns
-- the (possible) player at a given position. This function works on
-- any type of board.

playerAt ::
  Board
  -> Player

playerAt b = if length (_x  b) <= length (_o b) then X else O

-- `takeBack` *(optional)*: takes either a finished board or a board
-- in-play that has had at least one move and returns a board in-play.
-- It is a compile-time type error to call this function on an empty
-- board.

takeBack ::
  Board
  -> Either String Board

takeBack = undefined

-- `isDraw` *(optional)* if called on a game with fewer than 9
-- moves, a compile-time type-error results.

isDraw ::
  Board
  -> Either String Bool

isDraw = undefined


-- get moves done by the player by using corresponding lens
-- Board [] [] ^. playerMoves X
-- opponentMoves X .~ [(1, 2)] $ Board [(1, 1)] [] 
-- over (opponentMoves X) ((:) (3, 3)) $ Board [(1, 1)] [(2, 2)]

playerMoves ::
  Functor f =>
  Player
  -> ([Position] -> f [Position])
  -> Board
  -> f Board

playerMoves p =
  if p == X then x else o


-- the other player's moves

opponentMoves ::
  Functor f =>
  Player
  -> ([Position] -> f [Position])
  -> Board
  -> f Board

opponentMoves p =
  if p == X then o else x

gameFinished ::
  Board
  -> Bool

gameFinished b = length (_x b) + length (_o b) == 9

winningMoves ::
  [Position]
  -> Bool

winningMoves ps = undefined
  -- length ps >= 3 && 
  

window ::
  [a]
  -> [[a]]
window x = takeWhile ((== 3) . length) $ (take 3) `map` tails x

allEqual [] = False
allEqual (x:xs) = and $ (x ==) `map` xs
