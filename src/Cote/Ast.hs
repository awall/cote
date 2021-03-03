module Cote.Ast
    where

data Ast where
  AstBool :: Bool -> Ast
  AstInt :: Int -> Ast
  AstString :: String -> Ast
  AstSymbol :: String -> Ast
  AstCall :: String -> [Ast] -> [Ast] -> Ast
  AstBlock :: [Ast] -> Ast
