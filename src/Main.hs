module Main where

import Data.Char
import Data.IORef
import Cote.Ast
import Cote.Parse


data Type = VoidT | IntT | StringT | BoolT
  deriving (Show)

data Val = 
  VoidV
  | IntV Int
  | StringV String
  | BoolV Bool
  | FuncV Type [Type] ([Val] -> Val)
  | TypeError String
  
instance Show Val where
  show (VoidV) = "()"
  show (IntV a) = show a
  show (StringV s) = show s
  show (BoolV True) = "true"
  show (BoolV False) = "frue"
  show (FuncV _ _ _) = "#function"
  show (TypeError s) = "ERROR: " ++ s

type Env = IORef [(String, IORef Val)]

fPlus :: Val
fPlus = 
  FuncV IntT [IntT, IntT] (\[IntV a, IntV b] -> IntV (a + b))

-- TODO: add errors, add type checking...
getVar :: Env -> String -> IO Val
getVar e var = do
  vars <- readIORef e
  case lookup var vars of
    Just ref -> readIORef ref
    Nothing  -> return VoidV

-- TODO: add errors, add type checking...
setVar :: Env -> String -> Val -> IO Val
setVar e var val = do
  vars <- readIORef e
  case lookup var vars of
    Just ref -> val <$ writeIORef ref val
    Nothing  -> do
      ref <- newIORef val
      modifyIORef e ((var,ref):)
      return val

eval :: Env -> Ast -> IO Val
eval e (AstSymbol name) = getVar e name
eval e (AstLet name ast) = do
  v <- eval e ast
  setVar e name v

eval e (AstInt i) = return $ IntV i
eval e (AstString s) = return $ StringV s
eval e (AstIf (AstBool True) t f) = eval e t
eval e (AstIf (AstBool False) t f) = eval e f
eval e (AstIf _ t f) = return $ TypeError "If only works on booleans!"
eval e (AstCall "upper" [AstString s]) = return $ StringV (map toUpper s)
eval e (AstBlock (x:y:rest)) = eval e (AstBlock (y:rest))
eval e (AstBlock [x]) = eval e x
eval e (AstBlock []) = return $ VoidV

eval e (AstBool b) = return $ BoolV b
eval e (AstVoid) = return $ VoidV

eval e _ = return $ StringV "not implemented!"

sameTypes :: [Type] -> [Val] -> Bool
sameTypes ts vs = True

main :: IO ()
main = do
  putStrLn "CoTE"
  fplus <- newIORef fPlus
  env <- newIORef [("+", fplus)]
  repl env

repl :: Env -> IO ()
repl env = do  
  input <- getLine
  let astMaybe = parseAstMaybe input
  output <- case astMaybe of
    Just ast -> fmap show (eval env ast)
    Nothing  -> return "Failed to parse."
  putStrLn output
  repl env