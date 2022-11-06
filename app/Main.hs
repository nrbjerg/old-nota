module Main (main) where

import CLI
import Compile
import Files
import Parser

main :: IO ()
main = pipeline . file =<< parseCLIArgs

pipeline :: String -> IO ()
pipeline p = do
  file <- loadNotaFile p
  case parseFile file of
    (Left err) -> putStrLn err
    (Right ast) -> case compileAST ast of -- TODO: Do a step in between to do imports
      (Left err) -> print err
      (Right res) -> putStrLn res
