module Main (main) where

import CLI
import Files

main :: IO ()
main = print =<< loadNotaFile . file =<< parseCLIArgs
