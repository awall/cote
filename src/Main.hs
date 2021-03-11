module Main where

import Data.Char
import Data.IORef
import Cote.Ast
import Cote.Parse

type Closure = [(String, IORef Val)]

data Type = VoidT | IntT | StringT | BoolT
  deriving (Show)

data Val = 
  VoidV
  | IntV Int
  | StringV String
  | BoolV Bool
  | BuiltinV Type [Type] ([Val] -> IO Val)
  | FuncV Type [(String,Type)] Ast Closure
  | TypeError String
  
instance Show Val where
  show (VoidV) = "()"
  show (IntV a) = show a
  show (StringV s) = show s
  show (BoolV True) = "true"
  show (BoolV False) = "false"
  show (BuiltinV _ _ _) = "#builtin"
  show (FuncV _ _ _ _) = "#function"
  show (TypeError s) = "ERROR: " ++ s

type Env = IORef Closure

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

eval e (AstCall name args) = do 
  maybeF <- getVar e name
  case maybeF of
    FuncV _ fargs ast closure -> do
      newEnv <- newIORef closure
      let registerVar (v, (name, typ)) = do 
            x <- eval e v
            setVar newEnv name x
      mapM_ registerVar (zip args fargs)
      eval newEnv ast
    -- TODO: we need to do some type-checking methinks...
    BuiltinV _ _ f -> do
      vals <- mapM (eval e) args
      f vals
    VoidV -> return $ TypeError "Not found!"
    _ -> return $ TypeError "Not callable!"
    

eval e (AstFn args body) = do
  typedArgs <- mapM (\(name,t) -> return (name, IntT)) args -- TODO: we need to get types from the strings... should use a lookup
  typ <- return IntT -- TODO: we neeed to "type eval" the Ast to get this. or, just don't use it?
  closure <- readIORef e
  return $ FuncV typ typedArgs body closure

eval e (AstBlock [x]) = eval e x
eval e (AstBlock (y:rest)) = eval e y >> eval e (AstBlock rest)
eval e (AstBlock []) = return $ VoidV

eval e (AstIf pred t f) = do
  p <- eval e pred
  case p of
    BoolV True  -> eval e t
    BoolV False -> eval e f
    _           -> return $ TypeError "If only works on booleans!"


eval e (AstInt i) = return $ IntV i
eval e (AstString s) = return $ StringV s
eval e (AstBool b) = return $ BoolV b
eval e (AstVoid) = return $ VoidV

eval e _ = return $ StringV "not implemented!"

sameTypes :: [Type] -> [Val] -> Bool
sameTypes ts vs = True

fPlus, fEq, fLt, fGt, fAnd, fOr, fPrint :: Val
fPlus     = BuiltinV IntT  [IntT,  IntT]  (\[IntV  a, IntV  b] -> return $ IntV  (a + b))
fMult     = BuiltinV IntT  [IntT,  IntT]  (\[IntV  a, IntV  b] -> return $ IntV  (a * b))
fDiv      = BuiltinV IntT  [IntT,  IntT]  (\[IntV  a, IntV  b] -> return $ IntV  (a `div` b))
fSub      = BuiltinV IntT  [IntT,  IntT]  (\[IntV  a, IntV  b] -> return $ IntV  (a - b))
fEq       = BuiltinV BoolT [IntT,  IntT]  (\[IntV  a, IntV  b] -> return $ BoolV (a == b))
fLt       = BuiltinV BoolT [IntT,  IntT]  (\[IntV  a, IntV  b] -> return $ BoolV (a < b))
fGt       = BuiltinV BoolT [IntT,  IntT]  (\[IntV  a, IntV  b] -> return $ BoolV (a > b))
fAnd      = BuiltinV BoolT [BoolT, BoolT] (\[BoolV a, BoolV b] -> return $ BoolV (a && b))
fOr       = BuiltinV BoolT [BoolT, BoolT] (\[BoolV a, BoolV b] -> return $ BoolV (a || b))
fPrint    = BuiltinV VoidT [StringT]      (\[StringV s]        -> VoidV <$ putStrLn s)
fToString = BuiltinV VoidT [VoidT]        (\[x]                -> return $ StringV (show x))

main :: IO ()
main = do
  putStrLn "CoTE"
  fs <- builtins [
    ( "+", fPlus),
    ( "*", fMult),
    ( "/", fDiv),
    ( "-", fSub),
    ("==", fEq),
    ( "<", fLt),
    ( ">", fGt),
    ("&&", fAnd),
    ("||", fOr),
    ("print", fPrint),
    ("->string", fToString)]
  env <- newIORef fs
  repl env
  where builtins = mapM (\(name,f) -> (name,) <$> newIORef f)

repl :: Env -> IO ()
repl env = do  
  input <- getLine
  let astMaybe = parseAstMaybe input
  output <- case astMaybe of
    Just ast -> fmap show (eval env ast)
    Nothing  -> return "Failed to parse."
  putStrLn output
  repl env