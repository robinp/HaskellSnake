module Main (main) where

import Control.Monad ((<=<))

import Snake

stepConsole :: GameState -> IO GameState
stepConsole st = do
  input <- getLine
  let st' = newSt $ stepGame (Input $ dirFromString input) st
  (putStrLn . show . stSnake) st' 
  return st'
  where
    dirFromString s = if null s then Nothing else Just $ (read s :: Direction)

main = 
   main0 mkGame
   where
     main0 = main0 <=< stepConsole