module CursesMain (main) where

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad ((<=<))
import Data.Maybe

import UI.NCurses

import Snake

getInput cursesEvent = case cursesEvent of
  EventSpecialKey k -> case k of
    KeyUpArrow -> Just DUp
    KeyDownArrow -> Just DDown
    KeyLeftArrow -> Just DLeft
    KeyRightArrow -> Just DRight
    _ -> Nothing
  _ -> Nothing

updateSnake :: Snake -> Update ()
updateSnake s =
  let
    head = snakeHead s
    tail = snakeTail s
  in do
    mapM_ (updatePoint "*") $ tail
    updatePoint (dirToChar $ heading s) head
  where
    updatePoint chr p = do
      -- ugly hardcode, need to pass config
      moveCursor (20 - (toInteger $ py p)) (toInteger $ px p)
      drawString chr
    dirToChar dir = case dir of
      DDown -> "V"
      DUp -> "^"
      DLeft -> "<"
      DRight -> ">"

delSnakeTail :: Snake -> Update ()
delSnakeTail s =
  delPoint $ snakeEnd s
  where
    delPoint (Point x y) = do
      moveCursor (20 - (toInteger y)) (toInteger x)
      drawString " "

diffUpdate :: GameState -> Update ()
diffUpdate st = 
  let s = stSnake st
  in delSnakeTail s >> updateSnake s

updateAndRender w u =
  updateWindow w u >> render

loop :: Window -> GameState -> MaybeT Curses ()
loop w st = MaybeT $ do
  inputs <- fmap eventsToInput $ pullEvents w
  sleepSome
  let (Output st' _ maybeFinished) = stepGame (fmap Input inputs) st
  updateAndRender w $ diffUpdate st'
  maybe (runMaybeT $ loop w st') (const $ return Nothing) maybeFinished
  where
    sleepSome = liftIO $ threadDelay $ 1000 * 300

pullEvents :: Window -> Curses [Event]
pullEvents w = fmap reverse $ pullEvents0
  where 
    pullEvents0 = do     
      maybeEv <- getEvent w (Just 0)
      maybe (return []) (\ev -> fmap (ev:) pullEvents0) maybeEv

eventsToInput :: [Event] -> [Direction]
eventsToInput es = fmap getInput es >>= maybe [] return 

main = runCurses $ do
  setCursorMode CursorInvisible
  w <- newWindow 20 40 0 0
  updateWindow w $ do
    moveCursor 0 0
    drawString "Hello World"
    drawBox (Just glyphLineV) (Just glyphLineH)
  let st0 = mkGame
  updateAndRender w $ diffUpdate st0
  runMaybeT $ loop w st0
  closeWindow w
  