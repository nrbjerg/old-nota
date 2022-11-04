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
parseMacro = char '\\' *> (generalMacro <|> specialMacro <|> newlineMacro)
  where
    generalMacro = do
      id <- some letterChar
      args <- parseArguments
      return $ Macro id args
    specialMacro = do
      id <- oneOf "$@&;"
      return $ Macro [id] []
    newlineMacro = do
      _ <- string "\\\\"
      return NewlineMacro

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

-- Equation specific parsers
parseGroup :: Parser Node
parseGroup = do
  content <- parseEquationLine `between'` (char '{', char '}')
  return $ Group content

parseElementInEquation :: Parser Node
parseElementInEquation = do
  source <- some (letterChar <|> numberChar)
  return $ Element source

parseFraction :: Parser Node
parseFraction =
  let aux = parseGroup <|> parseMacro <|> parseElementInEquation
   in do
        numerator <- aux
        _ <- many spaceChar *> char '/' <* many spaceChar
        denominator <- aux
        return $ Fraction numerator denominator

parseSubscript :: Parser Node
parseSubscript = do
  _ <- char '_' *> many spaceChar
  content <- parseElementInEquation <|> parseGroup <|> parseMacro
  return $ Subscript content

parseRaised :: Parser Node
parseRaised = do
  _ <- char '^' *> many spaceChar
  content <- parseElementInEquation <|> parseGroup <|> parseMacro
  return $ Raised content

parseOperand :: Parser Node
parseOperand = do
  source <- oneOf "+-*=!|," -- TODO: Should | be apart of this?
  return $ Operand source

parseBracketPair :: Parser Node
parseBracketPair =
  let opening = oneOf "([" <|> (char '\\' *> char '{') -- TODO: Allow for having them the different way
      closing = oneOf ")];" <|> (char '\\' *> char '}')
   in do
        left <- opening
        content <- parseEquationLine
        right <- closing -- Semi colon is used for cases.
        return $ BracketPair left content right

parseEquationLine :: Parser [Node] -- FIXME: Fix this here.
parseEquationLine =
  many $
    many
      spaceChar
      *> ( try parseBracketPair <|> parseAmpersand <|> parseMacro <|> try parseFraction <|> parseGroup <|> parseSubscript <|> parseRaised <|> parseElementInEquation <|> parseOperand
         )

parseInline :: Parser Node -- Maybe use the between operator
parseInline = do
  content <- parseEquationLine `between'` (char '$', char '$') -- TODO: Allow for newlines
  return $ Inline content

parseInformation :: Parser Node
parseInformation = return $ Information "" []

parseHeader :: Parser Node
parseHeader = L.nonIndented scn p
  where
    p = do
      hashtags <- some $ char '#' -- FIXME throw parse error if there is to many hashtags
      content <- many (parseText <|> parseInline)
      return $ Header (length hashtags) content

-- Functions that are exported
parseComments :: Parser Node
parseComments = do
  _ <- char '%' *> many (noneOf "\n")
  return Comment

parser :: Parser Node
parser =
  parseMacro
    <|> parseComments
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
parseNota = many (parserWithNewline) <* eof

-- parseNota = L.nonIndented scn (many parser) <* eof

runParserOn :: String -> String
runParserOn str = case runParser parseNota "runParserOn" str of
  (Left err) -> show err
  (Right node) -> show node

parseFile :: Path -> IO ()
parseFile pathToFile = do
  file <- loadNotaFile pathToFile
  case runParser parseNota pathToFile $ file_content file ++ "\n" of
    (Left err) -> print err
    (Right node) -> print node
