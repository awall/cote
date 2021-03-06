module Cote.Ast
    where

data Ast where
  AstBool :: Bool -> Ast
  AstInt :: Int -> Ast
  AstString :: String -> Ast

  AstLet :: String -> Ast -> Ast
  AstSet :: String -> Ast -> Ast
  AstVoid :: Ast
  AstIf :: Ast -> Ast -> Ast -> Ast
  AstSymbol :: String -> Ast
  AstCall :: String -> [Ast] -> [Ast] -> Ast
  AstBlock :: [Ast] -> Ast
