-- | Contains the code related to compiling the AST to LaTeX
module Compile where

import AST
import Data.Either
import Files
import Parser

data CompileError = InvalidPath {line :: Int} deriving (Show)

compile :: Node -> Either CompileError String
compile n = Right $ show n

compileAST :: [Node] -> Either CompileError String
compileAST ast = aux ast ""
  where
    aux (node : rest) reg = case compile node of
      (Left err) -> Left err
      (Right res) -> aux rest (reg ++ res)
    aux _ res = Right res
