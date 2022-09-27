module Main (main) where

import CLI
import Files
import Options.Applicative

main :: IO ()
main = putStrLn . show =<< loadNotaFile . file =<< execParser opts
  where
    opts =
      info
        (cliArguments <**> helper)
        ( fullDesc
            <> progDesc "The nota compiler."
            <> header "The nota compiler"
        )
