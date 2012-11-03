module Main (main) where

import Control.Monad.Trans.Maybe
import Control.Monad ((<=<))

import Snake

stepConsole :: GameState -> MaybeT IO GameState
stepConsole st = MaybeT $ do
  input <- getLine
  let (Output st' _ maybeFinished) = stepGame (fmap Input $ dirFromString input) st
  (putStrLn . show . stSnake) st' 
  return $ maybe (Just st') (const Nothing) maybeFinished 
  where
    dirFromString s = if null s then [] else [read s :: Direction]

main = do
   result <- runMaybeT $ main0 mkGame
   putStrLn $ maybe "Finished" (\_ -> "?") result
   where
     main0 = main0 <=< stepConsole