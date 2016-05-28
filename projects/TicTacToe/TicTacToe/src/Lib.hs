{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

 module Lib
 --    ( move,
 --      whoWon,
 --      playerAt,
 --      takeBack,
 --      isDraw,
 --      emptyBoard,
 --      Player,
 --      Cell,
 --      Board, 
 --      X,
 --      O
 --    )
 where

import Data.Maybe (catMaybes, listToMaybe, isJust)
import Data.List
import Data.Functor
import qualified Data.Vector as V
import Control.Lens
import Data.Vector.Lens

-- | The position is a pair (row, column)
type Position = (Int, Int)

-- | The player is either 'X' or 'O'
data Player = X | O deriving (Eq, Show)

-- Create prisms _X and _O
makePrisms ''Player

-- | The cell is either empty or occupied by one of the players
data Cell = EmptyCell | Occupied Player deriving (Eq)

makePrisms ''Cell

instance Show Cell where
  show EmptyCell = "   "
  show (Occupied p) = if p == X then " X " else " O "

-- | The board is a list of lists of 'Cell's
newtype Board = Board [[Cell]] deriving (Eq)

-- | Print the board as familiar 3x3 grid
instance Show Board where
  show (Board b) = let
    n = length b
    w = n * 3 + (n - 1) 
    divider = "\n" ++ replicate w '-' ++ "\n"
    in
    intercalate divider $ showRow <$> b
    where
      showRow = intercalate "|" . map show

-- Since the 'Board' is a newtype, it creates
-- _Board :: Iso' Board [[Cell]]
-- to convert back and forth
makePrisms ''Board

-- | Empty board is full of 'EmptyCell's
emptyBoard :: Board
emptyBoard = Board [[EmptyCell | x <- [1..3]] | y <- [1..3]]

-- | A lens to get/set a cell at (row, column)
cell :: Applicative f =>
       Position -> (Cell -> f Cell) -> Board -> f Board
cell (x, y) = _Board . ix x . ix y

-- | 'move' takes a tic-tac-toe board and position and moves to that
-- position (if not occupied) returning a new board. This function can
-- only be called on a board that is empty or in-play. Calling `move`
-- on a game board that is finished is a *compile-time type error*.
--
-- If fewer than 5 moves have been played, then this
-- guarantees that the game is still in play, and so calling `move`
-- will never produce a type-error in this case.

move ::
  Position
  -> Board
  -> Board

move p b
  | boardFull b || (isJust $ whoWonSafe b) = error "Game finished"
  | b ^?! cell p /= EmptyCell               = error "Cell occupied"
  | otherwise = b & cell p .~ (Occupied $ playerAt b) 

-- | 'whoWon' takes a tic-tac-toe board and returns the player that
-- won the game (or a draw if neither). This function can only be
-- called on a board that is finished. Calling 'whoWon' on a game
-- board that is empty or in-play is a *compile-time type error*. As
-- an optional consideration, 'whoWon' should never be a draw if fewer
-- than nine moves have been played. In the case that the game is
-- completed, but fewer than nine moves have been played, return a
-- value that can only be one of two possibilities (the winner) and
-- never a draw.
-- @
-- let b = move (2, 2) . move (2, 0) . move (0, 0) . move (1, 2) . move (1, 1) $ emptyBoard
-- whoWon b
-- @

whoWon ::
  Board
  -> Maybe Player

whoWon b =
  case whoWonSafe b of
    Nothing -> if boardFull b then Nothing
              else
                error "Game not finished"
    mp -> mp
  

-- | 'whoWonSafe' return either Just Player if there is a winning line
-- or Nothing otherwise.
whoWonSafe ::
  Board
  -> Maybe Player

whoWonSafe b =  
  let
    size     = b ^. _Board . to length - 1
    byRow    = _Board . traverse
    imap2d f = iconcatMap f $ b ^.. byRow
    -- by row
    rows = b ^.. byRow ++
      -- by column
      ((\i ->  b ^.. _Board . traverse  . ix i) <$> [0 .. size]) ++
      -- diagonal 1
      [imap2d $ \i a -> [ a !! i]] ++
      -- diagonal 2
      [imap2d $ \i a -> [ a !! (size - i)]] 
  in listToMaybe . catMaybes $ winningLine <$> rows
  

-- | 'playerAt' takes a tic-tac-toe board and position and returns
-- the (possible) player at a given position. This function works on
-- any type of board.

playerAt ::
  Board
  -> Player

playerAt (Board b) = 
  if nof X <= nof O then X else O
  where
    nof p =
      foldr (\x acc -> case x of
                (Occupied p') -> if p' == p then 1 + acc else acc
                _ -> acc) 0 $ concat b
    
-- | 'takeBack' *(optional)*: takes either a finished board or a board
-- in-play that has had at least one move and returns a board in-play.
-- It is a compile-time type error to call this function on an empty
-- board.

takeBack ::
  Board
  -> Either String Board

takeBack = undefined

-- | 'isDraw' *(optional)* if called on a game with fewer than 9
-- moves, a compile-time type-error results.

isDraw ::
  Board
  -> Bool

isDraw b = if not $ boardFull b then
             error "Game not finished"
           else
             maybe True (const False) $ whoWon b 

-- allCells = _Board . each . each

boardFull ::
  Board
  -> Bool
boardFull = noneOf (_Board . each . each) (==EmptyCell)

gameFinished ::
  Board
  -> Bool
gameFinished b = (isJust . whoWonSafe) b || boardFull b

window ::
  [a]
  -> [[a]]
window x = takeWhile ((== 3) . length) $ (take 3) `map` tails x

allEqual [] = False
allEqual (x:xs) = and $ (x ==) `map` xs

winningLine ::
  [Cell]
  -> Maybe Player

winningLine cs =
  if allEqual cs then
    case head cs of
      Occupied p -> Just p
      _ -> Nothing
  else
    Nothing

validMoves ::
  Board
  -> [Position]

validMoves = undefined

-- | 'playMoves' applies the moves to the empty board. It does not
-- check if the game is finished.
playMoves ::
  [Position]
  -> Board

playMoves = foldr (\p b -> move p b) emptyBoard

-- | 'playMovesSafe' is the same as 'playMoves' but it can be used
-- with any board and it stops when the game is finished
playMovesSafe ::
  Board
  -> [Position]
  -> Board
playMovesSafe = foldr $ \p b -> if gameFinished b then b else move p b

-- | All moves available (todo: for the given board)
allMoves ::
  [Position]
allMoves = [(x, y) :: Position | x <- [0..2],  y <- [0..2]]
