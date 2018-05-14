{-# LANGUAGE OverloadedStrings #-}

module Language.Arith.Eval
  ( 
  -- * Run a file, string or expr 
    execFile
  , execString
  , execExpr

  -- * Convert string to AST
  , parse
  -- * Convert string to Tokens
  , tokens)
  where

import Control.Exception (throw, catch)
import Language.Arith.Types
import Language.Arith.Parser

--------------------------------------------------------------------------------
execFile :: FilePath -> IO () 
--------------------------------------------------------------------------------
execFile f = runFile f `catch` exitError

runFile :: FilePath -> IO ()
runfile f = do 
  s <- readFile f 
  n <- execString s
  putStrLn ("Result = " ++ show n) 

exitError :: Error -> IO () 
exitError (Error msg) = putStrLn ("Error: " ++ msg) 

--------------------------------------------------------------------------------
execString :: String -> IO () 
--------------------------------------------------------------------------------
execString s = execExpr (parseExpr s) `catch` exitError

--------------------------------------------------------------------------------
execExpr :: Expr -> IO () 
--------------------------------------------------------------------------------
execExpr e = return (eval [] e) `catch` exitError

--------------------------------------------------------------------------------
-- | `parse s` returns the Expr representation of the String s
--
-- >>> parse "123"
-- AConst 123
--
-- >>> parse "foo"
-- AVar "foo"
--
-- >>> parse "x + y"
-- APlus (AVar "x") (AVar "y")
--
-- >>> parse "10 - 2 - 2"
-- AMinus (AMinus (AConst 10) (AConst 2)) (AConst 2)
--
-- >>> parse "2 + 10 * 3"
-- APlus (AConst 2) (ATimes (AConst 10) (AConst 3)) 

--------------------------------------------------------------------------------
parse :: String -> Expr
--------------------------------------------------------------------------------
parse = parseExpr

--------------------------------------------------------------------------------
tokens :: String -> Either String [Token]
--------------------------------------------------------------------------------
tokens = parseTokens 