import Data.Char (isAlphaNum)
import Structures.Stack
import Text.Read (readMaybe)

data OP = Power | Modulo | Times | Divide | Plus | Minus deriving (Show, Eq)

data Token
  = Operator OP Int -- operation precedence
  | Variable String
  | Number Double
  | OpenParen
  | CloseParen
  deriving (Show, Eq)

power, modulo, times, divide, plus, minus :: Token
power = Operator Power 2
modulo = Operator Modulo 3
times = Operator Times 3
divide = Operator Divide 3
plus = Operator Plus 4
minus = Operator Minus 4

tokenize :: String -> [Token]
tokenize [] = []
tokenize (' ' : xs) = tokenize xs -- ignore spaces
tokenize ('(' : xs) = OpenParen : tokenize xs
tokenize (')' : xs) = CloseParen : tokenize xs
tokenize ('^' : xs) = power : tokenize xs
tokenize ('%' : xs) = modulo : tokenize xs
tokenize ('*' : xs) = times : tokenize xs
tokenize ('/' : xs) = divide : tokenize xs
tokenize ('+' : xs) = plus : tokenize xs
tokenize ('-' : xs) = minus : tokenize xs
tokenize xs =
  let term = takeWhile (\x -> isAlphaNum x || x == '.') xs
      rest = drop (length term) xs
   in if null term -- TODO: error handling
        then []
        else case readMaybe term :: Maybe Double of
          Nothing -> Variable term : tokenize rest
          Just d -> Number d : tokenize rest

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
