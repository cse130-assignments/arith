{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Language.Arith.Parser (
    parseExpr
  , parseTokens
  ) where

import Language.Arith.Lexer
import Language.Arith.Types hiding (Arith (..))
import Control.Monad.Except
import Control.Exception
}

-- Entry point
%name aexpr 

-- Lexer structure
%tokentype { Token }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    TNUM  { NUM _ $$ }
    ID    { ID _ $$  }
    '+'   { PLUS _   }
    '-'   { MINUS _  }
    '*'   { MUL _    }
    '/'   { DIV _    }
    '('   { LPAREN _ }
    ')'   { RPAREN _ }

-- Operators
%left '+' '-'
%left '*' '/'
%%

Aexpr : BinExp                     { $1           }
      | TNUM                       { AConst $1    }
      | ID                         { AVar   $1    }
      | LPAREN Expr RPAREN         { $2           }

BinExp : Expr MUL Expr             { AMul   $1 $3 }
       | Expr PLUS Expr            { APlus  $1 $3 } 
       | Expr MINUS Expr           { AMinus $1 $3 }
       | Expr DIV   Expr           { ADiv   $1 $3 }
{

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError []     = throwError "Unexpected end of Input"

parseExpr :: String -> Expr
parseExpr s = case parseExpr' s of
                Left msg -> throw (Error ("parse error:" ++ msg))
                Right e  -> e

parseExpr' input = runExcept $ do
   tokenStream <- scanTokens input
   aexpr tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens

}
