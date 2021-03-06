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
parseAstMaybe = parseMaybe (munch *> ast)

ast :: Parser Ast
ast = 
  astString 
  <|> astBlock
  <|> astCall 
  <|> astInt 
  <|> do w <- wordm
         case w of
           "true"  -> return $ AstBool True
           "false" -> return $ AstBool False
           "if"    -> astIfBody
           "let"   -> astLetBody
           "set"   -> astSetBody
           _       -> return $ AstSymbol w  

astInt :: Parser Ast
astInt = do
  digits <- many1 (oneOf "0123456789" <?> "digit")
  munch
  return $ AstInt (read digits)  

astBlock :: Parser Ast
astBlock = do
  charm '{'
  asts <- statements id
  return $ AstBlock asts
  where 
    statements f = do      
      a <- ast      
      c <- oneOf ";}" 
      munch
      if c == '}' 
         then return $ f [a]
         else do 
           end <- option False (True <$ charm '}')
           if end
              then return $ f [a, AstVoid]
              else statements (\x -> f (a:x))

astString :: Parser Ast
astString = do
  char '"'
  content <- many chars
  charm '"'
  return $ AstString content
  where
    chars = (char '\\' *> escaped) <|> noneOf ['"']
    escaped = choice $ map escape escapedChars
    escape (e,c) = c <$ char e
    escapedChars = 
      [('b', '\b')
      ,('n', '\n')
      ,('f', '\f')
      ,('r', '\r')
      ,('t', '\t')
      ,('\\', '\\')
      ,('"', '"')]

astCall :: Parser Ast
astCall = betweenm '[' ']' $ do
  name <- wordm
  templateArgs <- betweenm '<' '>' (many ast)
  functionArgs <- many ast
  return $ AstCall name templateArgs functionArgs
      
astIfBody :: Parser Ast
astIfBody = do
  condition <- ast
  trueBlock <- ast
  falseBlock <- option AstVoid (prefixm "else" *> ast)  
  return $ AstIf condition trueBlock falseBlock

astLetBody :: Parser Ast
astLetBody = do
  name <- wordm
  value <- ast  
  return $ AstLet name value

astSetBody :: Parser Ast
astSetBody = do
  name <- wordm
  value <- ast  
  return $ AstSet name value

betweenm :: Char -> Char -> Parser a -> Parser a
betweenm a b p = do
  charm a
  r <- p
  charm b
  return r

prefixm :: String -> Parser String
prefixm a = do
  s <- string a
  lookAhead (oneOf " \t\r\b\f\n<>[]{};")
  munch
  return s

wordm :: Parser String
wordm = do
  letters <- many1 $ noneOf " \t\r\b\f\n<>[]{};"
  munch
  return letters

charm :: Char -> Parser Char
charm a = do
  c <- char a
  munch
  return c

stringm :: String -> Parser String
stringm a = do
  s <- string a
  munch
  return a

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return $ x:xs

munch :: Parser String
munch = many $ oneOf " \t\r\b\f\n"