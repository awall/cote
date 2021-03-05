module Cote.Ast
    where

data Ast where
  AstVoid :: Ast
  AstIf :: Ast -> Ast -> Ast -> Ast
  AstBool :: Bool -> Ast
  AstInt :: Int -> Ast
  AstString :: String -> Ast
  AstSymbol :: String -> Ast
  AstCall :: String -> [Ast] -> [Ast] -> Ast
  AstBlock :: [Ast] -> Ast
