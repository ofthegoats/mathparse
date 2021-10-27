module Structures.Stack where

newtype Stack a = Stack [a] deriving (Show)

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack $ x : xs

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack []) = (Nothing, Stack [])
pop (Stack (x : xs)) = (Just x, Stack xs)

peek :: Stack a -> Maybe a
peek (Stack []) = Nothing
peek (Stack (x : xs)) = Just x

isEmpty :: Stack a -> Bool
isEmpty (Stack []) = True
isEmpty (Stack _) = False
