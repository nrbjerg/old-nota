-- | Contains logic for importing files into nota files
module Files where

import System.IO

data File = File
  { path :: String,
    file_content :: String
  }

instance Show File where
  show (File path file_content) = "[" ++ path ++ "]: \n" ++ file_content

type Path = String

loadNotaFile :: Path -> IO File
loadNotaFile file_path = do
  handle <- openFile file_path ReadMode
  content <- hGetContents handle
  return $ File file_path content
