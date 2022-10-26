{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Contains the logic for parsing the nota markup language
module Parser (runParserOn, parseFile) where

-- import Control.Monad (void)

import AST
import Control.Applicative (empty)
import Control.Monad
import Data.List
import Data.Maybe (isJust)
import Data.String
import Data.Void
import Files
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

lineComment :: Parser ()
lineComment = L.skipLineComment "%"

scn :: Parser ()
scn = L.space (void spaceChar) lineComment empty

sc :: Parser ()
sc = L.space (void $ oneOf " \t") lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parseEverythingButNewline :: Parser String
parseEverythingButNewline = lexeme . many $ noneOf "\n"

-- TODO: implement these functions
parseMacro :: Parser Node
parseMacro = do
  _ <- char '\\'
  id <- some (letterChar <|> oneOf "\\") -- TODO: Add more options (+ / ect.)
  args <- parseArguments
  return $ Macro id args

parseSemiColon :: Parser Node
parseSemiColon = do
  _ <- char ';'
  return SemiColon

parseAmpersand :: Parser Node
parseAmpersand = do
  _ <- char '&'
  return Ampersand

parseText :: Parser Node
parseText = do
  text <- some $ noneOf "#\\@;&{}$\n" -- TODO: Is this enough?
  return $ Text text

between' :: Parser a -> (Char, Char) -> Parser a
between' p (c1, c2) = do
  _ <- char c1
  content <- p
  _ <- char c2
  return content

parseArguments :: Parser [Node]
parseArguments = do
  args <- optional $ many (parseText <|> parseMacro <|> parseSemiColon) `between'` ('{', '}')
  return $ aux args
  where
    aux (Just actual_args) = actual_args
    aux Nothing = []

parseLatex :: Parser Node
parseLatex = L.indentBlock scn p
  where
    p = do
      _ <- string "@latex:"
      return $ L.IndentMany Nothing (return . (Latex . aux)) parseEverythingButNewline
      where
        aux = foldl (\x y -> x ++ y ++ "\n") ""

parseEnvironment :: Parser Node -- FIXME: This is very buggy
parseEnvironment = parseLatex <|> L.indentBlock scn p
  where
    p = do
      _ <- char '@'
      env <- some letterChar -- TODO: Fail if this is "latex"
      args <- parseArguments
      _ <- char ':'
      case env of
        "eq" -> return $ L.IndentMany Nothing (return . Equation args . concat) parseEquationLine
        _ -> return $ L.IndentMany Nothing (return . Environment env args . concat) (many parser) -- Something goes wrong here.

parseEquationLine :: Parser [Node]
parseEquationLine =
  many
    ( parseAmpersand <|> parseText <|> parseMacro
    )

parseInline :: Parser Node -- Maybe use the between operator
parseInline = do
  contents <- (parseEquationLine) `between'` ('$', '$') -- TODO: Allow for newlines
  return $ Inline contents

parseInformation :: Parser Node
parseInformation = return $ Information "" []

parseHeader :: Parser Node
parseHeader = L.nonIndented scn p
  where
    p = do
      hashtags <- some $ char '#' -- FIXME throw parse error if there is to many hashtags
      contents <- many (parseText <|> parseInline)
      return $ Header (length hashtags) contents

-- Functions that are exported
parser :: Parser Node
parser =
  lexeme $
    ( parseEnvironment
        <|> parseMacro
        <|> parseText
        <|> parseInline
        <|> parseHeader
        --   <|> parseInformation
    )

-- NOTE: We could also define: parseNota = some parser <* eof
parseNota :: Parser [Node]
parseNota = L.nonIndented scn (many parser) <* eof

runParserOn :: String -> String
runParserOn str = case runParser parseNota "test" str of
  (Left err) -> show err
  (Right node) -> show node

parseFile :: Path -> IO ()
parseFile pathToFile = do
  file <- loadNotaFile pathToFile
  case runParser parseNota pathToFile $ file_contents file of
    (Left err) -> print err
    (Right node) -> print node
