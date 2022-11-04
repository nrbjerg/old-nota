-- | Contains the datastructures needed for implementing the AST of Nota
module AST where

data Node
  = Macro {id :: String, args :: [Node]}
  | Latex {source :: String}
  | Environment {id :: String, args :: [Node], content :: [Node]}
  | Text {source :: String}
  | -- Header & information
    Information {id :: String, content :: [Node]}
  | Header {level :: Int, content :: [Node]}
  | Newline
  | Comment
  | NewlineMacro
  | Equation {args :: [Node], content :: [Node]} -- TODO: the source code should also be parsed, however this will have to wait.
  | Inline {content :: [Node]}
  | -- Special things for equations
    Ampersand
  | SemiColon
  | Operand {typ :: Char}
  | Element {source :: String} -- This is either a character or a number
  | Subscript {element_or_group :: Node} --
  | Raised {element_or_group :: Node}
  | Group {content :: [Node]}
  | Factor {content :: [Node]}
  | BracketPair {left :: Char, content :: [Node], right :: Char}
  | Fraction {numerator :: Node, denominator :: Node} -- Each one of these is either a text object, macro or a group
  deriving (Show, Eq)
