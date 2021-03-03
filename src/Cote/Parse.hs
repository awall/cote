--
-- After some quick comparisons, settled on using Megaparsec for parsing.
-- See https://github.com/mrkkrp/megaparsec#comparison-with-other-solutions for comparison of alternatives.
--
module Cote.Parse
  ( parseAstMaybe
  ) where

import Control.Monad.State
import Data.Char
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

import Cote.Ast


type Parser = Parsec Void String

parseAstMaybe :: String -> Maybe Ast
parseAstMaybe = parseMaybe (spaced ast)

ast :: Parser Ast
ast = 
  astString 
  <|> astBlock
  <|> astCall 
  <|> astInt 
  <|> astSymbolOrBool

astInt :: Parser Ast
astInt = AstInt <$> read <$> many1 digit

astBlock :: Parser Ast
astBlock = AstBlock <$> betweenChars '{' '}' (spacedMany (ast <* char ';'))

astString :: Parser Ast
astString =
  AstString <$> betweenChars '"' '"' (many chars)
    where
      chars = (char '\\' >> escaped) <|> noneOf ['"']
      escaped = choice $ map escape escapedChars
      escape (e,c) = char e >> return c
      escapedChars = 
        [('b', '\b')
        ,('n', '\n')
        ,('f', '\f')
        ,('r', '\r')
        ,('t', '\t')
        ,('\\', '\\')
        ,('"', '"')]

astCall :: Parser Ast
astCall = 
  betweenChars '[' ']' $ do
    name <- spaced word
    templateArgs <- betweenChars '<' '>' (spacedMany ast)
    functionArgs <- spacedMany ast
    return $ AstCall name templateArgs functionArgs

astSymbolOrBool :: Parser Ast
astSymbolOrBool = 
  toAst <$> word
    where 
      toAst w = 
        case w of
          "true" -> AstBool True
          "false" -> AstBool False
          _ -> AstSymbol w  

word :: Parser String
word = many1 $ noneOf " \t\r\b\f\n<>[]{};"

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return (x:xs)

whitespace :: Parser Char
whitespace = oneOf " \t\r\b\f\n"

digit :: Parser Char
digit = oneOf "0123456789"

munch :: Parser ()
munch = void $ many $ whitespace

spaced :: Parser a -> Parser a
spaced p = munch *> p <* munch

spacedMany :: Parser a -> Parser [a]
spacedMany p = munch *> p `sepEndBy` munch

betweenChars :: Char -> Char -> Parser a -> Parser a
betweenChars s e = between (char s) (char e)