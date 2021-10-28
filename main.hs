import RPN
import Token

-- represent a token as a string
prettyToken :: Token -> String
prettyToken t = case t of
  Operator op _ -> case op of
    Power -> "^"
    Modulo -> "%"
    Times -> "*"
    Divide -> "/"
    Plus -> "+"
    Minus -> "-"
  Variable v -> v
  Number d -> show d
  OpenParen -> "("
  CloseParen -> ")"

-- given an infix string, make a readable RPN representation of the same input
showRPN :: String -> String
showRPN input = unwords $ prettyToken <$> (buildRPN . tokenize) input
