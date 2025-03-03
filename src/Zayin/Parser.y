{
module Zayin.Parser where

import Control.Monad.Except
import Data.Text as T
import Zayin.Lexer.Support
import Zayin.AST
import Zayin.Lexer (scan)

}

%name parseExpr Expr
%name parseDef Def

%tokentype { Token }
%monad { Lexer }
%lexer { lexer } { TkEOF }

%errorhandlertype explist
%error { parseError }

%token
  VAR     { TkIdent $$ }
  'let'   { TkLet }

  '='     { TkEqual }
  '{'     { TkOpen }
  ';'     { TkSemi }
  '}'     { TkClose }
  '\\'    { TkBackslash }
  '->'    { TkArrow }

  '('     { TkLParen }
  ')'     { TkRParen }

  OPEN    { TkVOpen }
  SEMI    { TkVSemi }
  CLOSE   { TkVClose }

%%

Atom :: { Expr }
  : VAR { EVar (T.pack $1) }
  | '(' Expr ')' { $2 }

Expr :: { Expr }
  : '\\' VAR '->' Expr { ELam [(T.pack $2)] $4 }
  | 'let' DefBlock { Let $2 }
  | FuncExpr                  { $1 }

FuncExpr :: { Expr }
  : FuncExpr Atom { App $1 $2 }
  | Atom          { $1 }

DefBlock :: { [ExprBodyExpr] }
  : '{' DefListSemi '}'    { $2 }
  | OPEN DefListSEMI Close { $2 }

DefListSemi :: { [ExprBodyExpr] }
  : Def ';' DefListSemi { $1:$3 }
  | Def                  { [$1] }
  | {- empty -}           { [] }

DefListSEMI :: { [ExprBodyExpr] }
  : Def SEMI DefListSemi { $1:$3 }
  | Def                  { [$1] }
  | {- empty -}            { [] }

Close
  : CLOSE { () }
  | error {% popLayout }

Def
: VAR '=' Def { $1 $3 }
{
lexer cont = scan >>= cont

parseError = throwError . show
}
