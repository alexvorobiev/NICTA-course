{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Property

-- import System.IO.Unsafe
import Control.Exception (evaluate)

import Control.Monad (filterM)
-- import Control.Monad.State.Lazy
-- import Control.Spoon (teaspoon)
import Data.Maybe (isJust)
import Data.List (permutations, nub)

import Control.Lens
import Lib

main :: IO ()
main = hspec $ do
  
  describe "move" $ do
    it "inserts in correct position" $ do
      move (1, 1) emptyBoard ^?! cell (1, 1) `shouldBe` Occupied X

    it "inserts in correct arbitrary position" $ do
      property $ \p -> move p emptyBoard ^?! cell p == Occupied X

    it "fails on the move to an occupied cell" $ do
      property $ \p -> evaluate ((move p . move p) emptyBoard) `shouldThrow`
        errorCall "Cell occupied"

    it "throws an error if somebody won" $ do
      property $ \p -> evaluate (move p $ winningBoard) `shouldThrow`
        errorCall "Game finished"

    it "throws an error on full board" $ do
      property $ \p -> evaluate (move p $ fullBoard) `shouldThrow`
        errorCall "Game finished"

    it "less than 5 moves result in in-play board" $ do
      forAll genMoves $
        \x -> length x < 5 ==> evaluate (whoWon $ playMoves x) `shouldThrow`
              errorCall "Game not finished"
      
  describe "whoWon" $ do
    it "determines who won correctly" $ do
      whoWon winningBoard `shouldBe` Just X

    it "throws an error on empty board" $ do
      evaluate (whoWon emptyBoard) `shouldThrow` errorCall "Game not finished"

    it "throws an error on incomplete board" $ do
      evaluate (whoWon $ move (0, 0) . move (2, 2) $ emptyBoard) `shouldThrow`
        errorCall "Game not finished"

    it "should never be a draw if fewer than nine moves have been played" $ do
      forAll genMoves $
        \x -> length x < 9 ==>
              let game = whoWon $ playMoves x in 
                (evaluate game `shouldThrow` errorCall "Game not finished")
                .||.
                (evaluate game `shouldThrow` errorCall "Game finished")
                .||.
                (isJust game) -- draw results in Nothing

    it "finished game with fewer than nine moves should have a winner" $ do
      forAll genMoves $
        \x -> let b = playMovesSafe emptyBoard x in
          (length (b ^.. _Board . each . each . filtered (/= EmptyCell)) < 9 &&
                   gameFinished b) ==>
              isJust $ whoWon b

  describe "playerAt" $ do
    it "returns correct result based on number of occupied cells" $ do
      forAll genMoves $
        \x -> let b = playMovesSafe emptyBoard x in
          playerAt b ==
          if odd $ length (b ^.. _Board . each . each . filtered (/= EmptyCell)) then
            O
          else X

  describe "isDraw" $ do
    it "if called on a game with fewer than 9 moves, a compile-time type-error results." $ do
      forAll genMoves $
        \x -> let b = playMovesSafe emptyBoard x in
                length (b ^.. _Board . each . each . filtered (/= EmptyCell)) < 9 ==>
                evaluate (isDraw b) `shouldThrow` errorCall "Game not finished"
      
      
-- Position is a type synonym so TypeSynonymInstances is needed
instance Arbitrary Position where
  arbitrary = do
    r <- choose (0, 2)
    c <- choose (0, 2)
    return (r, c)

-- An example of a winning board
winningBoard = playMoves [(0, 0), (1, 0), (1, 1), (0, 1), (2, 2)] 

fullBoard = playMoves
  [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 1), (2, 0), (2, 2)]  

-- powerset courtesy of LYAH
powerset ::
  [a]
  -> [[a]]

powerset =
  filterM $ const [True, False]

-- Generate a sequence of moves
genMoves :: Gen [Position]
genMoves = Test.QuickCheck.elements $ powerset allMoves

-- List of all end-game boards
-- allGames ::
--   [Board]
-- allGames =
--   let allSeq = concatMap permutations $ powerset $ allMoves in
--     nub $ movesGame <$> allSeq

  
-- instance Arbitrary Board where
  -- arbitrary = do

-- True if the code throws an error
-- throwsError :: a -> Bool
-- throwsError = isNothing . teaspoon

-- spoon library
-- Catch the error and convert it to Nothing. NOINLINE is needed for unsafePerformIO
-- http://stackoverflow.com/a/4248650


-- {-# NOINLINE unsafeCleanup #-}
-- unsafeCleanup :: a -> Maybe a
-- unsafeCleanup x = unsafePerformIO $ Exc.catch (x `seq` return $ Just x) handler
--   where
--     handler e = return Nothing `const` (e :: Exc.ErrorCall)
    
