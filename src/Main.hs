module Main where

import Data.Char
import Cote.Ast
import Cote.Parse


data Val = VoidV | IntV Int | StringV String | BoolV Bool | TypeError String
  deriving (Show)


main :: IO ()
main = do
  putStrLn "CoTE"
  repl

eval :: Ast -> Val
eval (AstInt i) = IntV i
eval (AstString s) = StringV s
eval (AstIf (AstBool True) t f) = eval t
eval (AstIf (AstBool False) t f) = eval f
eval (AstIf _ t f) = TypeError "If only works on booleans!"
eval (AstCall "upper" [AstSymbol "string"] [AstString s]) = StringV $ map toUpper s
eval (AstBlock (x:y:rest)) = eval (AstBlock (y:rest))
eval (AstBlock [x]) = eval x
eval (AstBlock []) = VoidV
eval (AstBool b) = BoolV b
eval (AstVoid) = VoidV
eval _ = StringV "not implemented!"

repl :: IO ()
repl = do
  input <- getLine
  let astMaybe = parseAstMaybe input
  let output = 
        case astMaybe of
          Just ast -> show (eval ast)
          Nothing -> "Failed to parse."
  putStrLn output
  repl