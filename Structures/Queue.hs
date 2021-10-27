module Structures.Queue where

newtype Queue a = Queue [a] deriving (Show)

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue xs) = Queue $ xs ++ [x]

dequeue :: Queue a -> Queue a
dequeue (Queue []) = Queue [] -- pattern for empty queue
dequeue (Queue xs) = Queue $ tail xs

isEmpty :: Queue a -> Bool
isEmpty (Queue []) = True
isEmpty _ = False
