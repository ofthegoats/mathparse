module RPN where

import Structures.Stack
import Token

-- given an /infix/ list of tokens, made it /postfix/
-- using the shunting-yard algorithm
-- NOTE: currently /assuming/ input to be valid
buildRPN :: [Token] -> [Token]
buildRPN tokens = buildRPN' tokens $ Stack []
  where
    buildRPN' :: [Token] -> Stack Token -> [Token]
    buildRPN' [] (Stack s) = s
    buildRPN' (t : ts) s = case t of
      Number _ -> t : buildRPN' ts s
      OpenParen -> buildRPN' ts $ push OpenParen s
      CloseParen -> case pop s of
        (Nothing, _) -> [] -- TODO: error handling
        (Just OpenParen, s') -> buildRPN' ts s'
        (Just o', s') -> o' : buildRPN' (t : ts) s'
      Operator o1 preo1 -> case pop s of
        (Just (Operator o2 preo2), s') ->
          if preo2 <= preo1
            then Operator o2 preo2 : buildRPN' (t : ts) s'
            else buildRPN' ts $ push (Operator o1 preo1) s
        (_, s') -> buildRPN' ts $ push (Operator o1 preo1) s
