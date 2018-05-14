module Language.Nano.Types where

import           Control.Exception

data Error = Error {errMsg :: String}
             deriving (Show, Typeable)

instance Exception Error

data Aexpr =
  | AConst  Int
  | AVar    String
  | APlus   Aexpr Aexpr
  | AMinus  Aexpr Aexpr
  | ATimes  Aexpr Aexpr
  | ADivide Aexpr Aexpr
  deriving (Show)

type Env = [(String, Int)] 

type Value = Int 

eval :: Env -> Aexpr -> Value 
eval env (AConst i)      = i 
eval env (AVar   x)      = List.assoc s env
eval env (APlus  e1 e2)  = eval env e1 + eval env e2
eval env (AMinus e1 e2)  = eval env e1 - eval env e2
eval env (ATimes e1 e2)  = eval env e1 * eval env e2
eval env (ADivide e1 e2) = eval env e1 / eval env e2