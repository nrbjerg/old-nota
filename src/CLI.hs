-- | This module defines the command line interface for the nota compiler
module CLI where

import Options.Applicative

data CLIArguments = CLIArguments
  { file :: String,
    quiet :: Bool
  }
  deriving (Show)

cliArguments :: Parser CLIArguments
cliArguments =
  CLIArguments
    <$> argument
      str
      (metavar "FILE" <> help "The Nota source file to compile.")
    <*> switch
      ( long "quiet"
          <> short 'q'
          <> help "Whether to be quiet"
      )

parseCLIArgs :: IO CLIArguments
parseCLIArgs = execParser opts
  where
    opts =
      info
        (cliArguments <**> helper)
        ( fullDesc
            <> progDesc "The nota compiler."
            <> header "The nota compiler"
        )
