{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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
  id <- some (letterChar <|> oneOf "\\-+&") -- TODO: Add more options (+ / ect.)
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
  text <- some $ noneOf "#\\@;%&{}$\n" -- TODO: Is this enough?
  return $ Text text

between' :: Parser a -> (Parser b, Parser b) -> Parser a
between' contentParser (start, end) = do
  _ <- start
  content <- contentParser
  _ <- end
  return content

parseArguments :: Parser [Node]
parseArguments = do
  args <- optional $ many (parseText <|> parseMacro <|> parseSemiColon) `between'` (char '{', char '}')
  return $ aux args
  where
    aux (Just actual_args) = actual_args
    aux Nothing = []

parseLatex :: Parser Node
parseLatex = char '@' *> L.indentBlock scn p
  where
    p = do
      _ <- string "latex:"
      _ <- many $ oneOf "\t "
      return $ L.IndentMany Nothing (return . (Latex . aux)) parseEverythingButNewline
      where
        aux = foldl (\x y -> x ++ y ++ "\n") ""

parseEnvironment :: Parser Node -- FIXME: This is very buggy
parseEnvironment = char '@' *> L.indentBlock scn p
  where
    p = do
      env <- some letterChar <|> fail "Expected enviroment id with letters in the range [a-z]"
      args <- parseArguments
      _ <- char ':'
      _ <- many $ oneOf "\t "
      case env of
        "eq" -> do
          contentsOfFirstLine <- parseEquationLine
          return $ L.IndentMany Nothing (return . Equation args . (contentsOfFirstLine ++) . concat) parseEquationLine
        _ -> do
          contentsOfFirstLine <- many parser
          return $ L.IndentMany Nothing (return . Environment env args . (contentsOfFirstLine ++) . concat) $ many parser -- Something goes wrong here.

parseSymbol :: Parser Node -- TODO.
parseSymbol = return SemiColon

parseEquationLine :: Parser [Node] -- FIXME: Fix this here.
parseEquationLine =
  many
    ( parseAmpersand <|> parseText <|> parseMacro
    )

parseInline :: Parser Node -- Maybe use the between operator
parseInline = do
  contents <- parseEquationLine `between'` (char '$', char '$') -- TODO: Allow for newlines
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
  parseMacro
    <|> parseText
    <|> parseInline
    <|> try parseHeader
    <|> try parseLatex
    <|> parseEnvironment

parseNewline :: Parser Node
parseNewline = do
  _ <- char '\n'
  return Newline

parserWithNewline :: Parser Node
parserWithNewline = parser <|> parseNewline

parseNota :: Parser [Node]
parseNota = many (lexeme parserWithNewline) <* eof

-- parseNota = L.nonIndented scn (many parser) <* eof

runParserOn :: String -> String
runParserOn str = case runParser parseNota "runParserOn" str of
  (Left err) -> show err
  (Right node) -> show node

parseFile :: Path -> IO ()
parseFile pathToFile = do
  file <- loadNotaFile pathToFile
  case runParser parseNota pathToFile $ file_contents file ++ "\n" of
    (Left err) -> print err
    (Right node) -> print node
