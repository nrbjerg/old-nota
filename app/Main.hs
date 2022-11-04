module Main (main) where

import CLI
-- import Files
import Parser

main :: IO ()
main = parseFile . file =<< parseCLIArgs
