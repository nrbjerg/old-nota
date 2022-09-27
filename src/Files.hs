-- | Contains logic for importing files into nota files
module Files where

import System.IO

data File = File
  { path :: String,
    contents :: String
  }

instance Show File where
  show (File path contents) = "[" ++ path ++ "]: \n" ++ contents

type Path = String

loadNotaFile :: Path -> IO File
loadNotaFile file_path = do
  handle <- openFile file_path ReadMode
  contents <- hGetContents handle
  return $ File file_path contents
