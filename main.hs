import Data.Char (isAlphaNum)
import Structures.Queue
import Structures.Stack
import Text.Read (readMaybe)

data OP = Power | Modulo | Times | Divide | Plus | Minus deriving (Show, Eq)

data Token
  = Operator OP Int
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
   in case readMaybe term :: Maybe Double of
        Nothing -> Variable term : tokenize rest
        Just d -> Number d : tokenize rest
