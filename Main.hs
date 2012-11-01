module Main (main) where

import Queue
import Utils

import Control.Monad ((<=<))
import Control.Monad.Trans.Reader
import Data.Maybe (fromJust, fromMaybe)

data Point = Point { px :: Int, py :: Int } deriving (Eq, Show)

data Direction = DLeft | DRight | DUp | DDown deriving (Eq, Show, Read)

opposite dir = case dir of
  DLeft  -> DRight
  DRight -> DLeft
  DUp    -> DDown
  DDown  -> DUp

offset :: Direction -> Point -> Point
offset dir (Point x y) = case dir of
  DLeft  -> Point (x-1) y
  DRight -> Point (x+1) y
  DUp    -> Point x (y+1)
  DDown  -> Point x (y-1)

data Snake = Snake { 
  body :: Queue Point, 
  heading :: Direction 
}

instance Show Snake where
  show s = show (qToList $ body s) ++ " heading " ++ show (heading s)

advance :: Snake -> Snake
advance = do
  points <- body 
  dir <- heading
  let extendedPoint = offset dir $ fromJust $ lastOfQ points
  let newPoints = fst $ deq $ enq extendedPoint points
  return $ Snake newPoints dir
  
dirChange :: Snake -> Direction -> Maybe Snake
dirChange snake dir =
  justIfNot (opposite dir == heading snake) snake'
  where
    snake' = snake { heading = dir }
    
data Log = String
data Input = Input { inDirChange :: Maybe Direction }
data GameState = GameState { stSnake :: Snake } 
data Output = Output { newSt :: GameState, logs :: [Log], result :: Maybe () }

stepGame :: Input -> GameState -> Output
stepGame input st =
  let
    snake = stSnake st
    snake' = advance $ fromMaybe snake $ inDirChange input >>= dirChange snake
  in
   Output (GameState snake') [] Nothing

stepConsole :: GameState -> IO GameState
stepConsole st = do
  input <- getLine
  let st' = newSt $ stepGame (Input $ dirFromString input) st
  (putStrLn . show . stSnake) st' 
  return st'
  where
    dirFromString s = if null s then Nothing else Just $ (read s :: Direction)

main = 
   main0 s0
   where
     s0 = GameState $ Snake (qFromList [Point 1 1, Point 2 1]) DRight
     main0 = main0 <=< stepConsole

