module Main where

import Control.Monad.State
import System.IO (hFlush, stdout)
import Lib

main :: IO ()
main = do
  putStrLn "Hello"
  r <- evalStateT mainLoop emptyBoard
  print r
  putStrLn "Bye-bye!"

mainLoop :: StateT Board IO ()
mainLoop = do
  b <- get
  liftIO $ do
    putStrLn ""
    print b
    putStrLn ""
    putStr "Command (1..9, n, q): "
    hFlush stdout

  c <- liftIO getLine
  if c == "q" then
    return ()
    else do
      if c == "n" then do
        liftIO $ putStrLn "New Game!"
        put emptyBoard
        else
        if length c == 1 && (head c) `elem` ['1'..'9'] then 
          modify $ move $ allMoves !! ((read c) - 1)
        else
          liftIO $ putStrLn "Valid commands are 1..9, n, q"
      mainLoop 
