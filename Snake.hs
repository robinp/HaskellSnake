module Snake (
  Direction(..),
  Point(..),
  
  Snake,
  heading,
  snakeHead,
  snakeEnd,
  snakeTail,
  
  Input(..),
  Output(..),
  
  GameState(..),
  mkGame,
  stepGame,
  ) where

import Queue
import Utils

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

snakeHead :: Snake -> Point
snakeHead s = fromJust $ lastOfQ $ body s

snakeEnd :: Snake -> Point
snakeEnd s = fromJust $ snd $ deq $ body s

snakeTail :: Snake -> [Point]
snakeTail s = tail $ qToList $ body s 

instance Show Snake where
  show s = show (qToList $ body s) ++ " heading " ++ show (heading s)

advance :: Snake -> Snake
advance = do
  points <- body 
  dir <- heading
  hd <- snakeHead
  let extendedPoint = offset dir hd
  let (newPoints, droppedEnd) = deq $ enq extendedPoint points
  return $ Snake newPoints dir
       
dirChange :: Snake -> Direction -> Maybe Snake
dirChange snake dir =
  justIfNot (opposite dir == heading snake) snake'
  where
    snake' = snake { heading = dir }

collidesWithSelf :: Snake -> Bool
collidesWithSelf s = 
  let points = reverse $ qToList $ body s
  in head points `elem` tail points

data Log = String
data Input = Input { inDirChange :: Direction }
data GameState = GameState { stSnake :: Snake } 
data Output = Output { newSt :: GameState, logs :: [Log], result :: Maybe () }

mkGame :: GameState
mkGame = GameState $ Snake (qFromList $ fmap (flip Point 3) [1..5]) DRight

stepGame :: [Input] -> GameState -> Output
stepGame inputs st =
  let
    snake = stSnake st
    -- keep last only, we can do it for now since inputs are simple
    input = justIfNot (null inputs) (last inputs)
    snake' = advance $ fromMaybe snake $ fmap inDirChange input >>= dirChange snake
    finished = justIf (collidesWithSelf snake') ()
  in
   Output (GameState snake') [] finished


