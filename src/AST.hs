-- | Contains the datastructures needed for implementing the AST of Nota
module AST where

data Node
  = Macro {id :: String, args :: [Node]}
  | Latex {source :: String}
  | Environment {id :: String, numbered :: Bool, args :: [Node], contents :: [Node]}
  | Text {source :: String}
  | Comma
  | Equation {numbered :: Bool, contents :: [Node]} -- TODO: the source code should also be parsed, however this will have to wait.
  | Inline {contents :: [Node]}
  | Header {level :: Int, contents :: [Node]} -- NOTE: A header may contain inline equations.
  | Information {id :: String, contents :: [Node]}
  deriving (Show, Eq)
