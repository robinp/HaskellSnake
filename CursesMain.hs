module CursesMain (main) where

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad ((<=<))
import Data.Maybe

import UI.NCurses

import Queue

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
    bodyPoints = body s
    head = fromJust $ lastOfQ bodyPoints
    tails = tail $ qToList bodyPoints
  in do
    mapM_ (updatePoint "*") $ tails
    updatePoint (dirToChar $ heading s) head
  where
    updatePoint chr p = do
      moveCursor (20 - (toInteger $ py p)) (toInteger $ px p)
      drawString chr
    dirToChar dir = case dir of
      DDown -> "V"
      DUp -> "^"
      DLeft -> "<"
      DRight -> ">"

delSnakeTail :: Snake -> Update ()
delSnakeTail s =
  maybe (return ()) delPoint $ snd $ deq $ body s
  where
    delPoint (Point x y) = do
      moveCursor (20 - (toInteger y)) (toInteger x)
      drawString " "

loop :: Window -> GameState -> Curses ()
loop w st = do
  updateWindow w $ updateSnake $ stSnake st
  render
  inputs <- fmap eventsToInput $ pullEvents w
  sleepSome
  let st' = newSt $ stepGame (fmap Input inputs) st
  updateWindow w $ delSnakeTail $ stSnake $ st 
  render
  loop w st'
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
  updateWindow  w $ do
    moveCursor 0 0
    drawString "Hello World"
    drawBox (Just glyphLineV) (Just glyphLineH)
  render
  loop w mkGame
  closeWindow w
  