module Queue (
  Queue, 
  emptyQ, 
  enq, 
  deq, 
  lastOfQ, 
  qToList, 
  qFromList
) where

import Utils

data Queue a = Queue [a] [a] (Maybe a)

emptyQ :: Queue a
emptyQ = Queue [] [] Nothing

deq :: Queue a -> (Queue a, Maybe a)
deq q = case q of 
  Queue (x:xs) ys last       -> (Queue xs ys $ dropIf (bothEmpty xs ys) last, Just x)
  Queue [] (xs @ (_:_)) last -> deq $ Queue (reverse xs) [] last
  _                          -> (q, Nothing)
  
enq :: a -> Queue a -> Queue a
enq y (Queue xs ys last) = Queue xs (y:ys) (Just y)

lastOfQ :: Queue a -> Maybe a
lastOfQ (Queue _ _ last) = last

qToList :: Queue a -> [a]
qToList (Queue xs ys _) = xs ++ reverse ys

qFromList :: [a] -> Queue a
qFromList xs = Queue xs [] $ justIfNot (null xs) $ last xs
