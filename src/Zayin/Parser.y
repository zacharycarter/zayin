{
module Zayin.Parser
  ( parseProgram
  , parseWithDebug
  ) where

import qualified Data.Text as T
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Debug.Trace

import Zayin.Lexer
import Zayin.Lexer.Support
import Zayin.AST
import Zayin.Literals
}

%name parseExpr Expr
%name parseTopLevel Program

%tokentype { Token }
%monad { Lexer }
%lexer { lexer } { TkEOF }

%error { parseError }

%token
  ident       { TkIdent $$ }
  intlit      { TkIntLit $$ }
  strlit      { TkStringLit $$ }
  let         { TkLet }
  in          { TkIn }
  where       { TkWhere }
  if          { TkIf }
  else        { TkElse }
  fn          { TkFn }
  true        { TkTrue }
  false       { TkFalse }
  nil         { TkNil }
  not         { TkNot }
  macro       { TkMacro }
  '='         { TkEqual }
  '{'         { TkOpen }
  '}'         { TkClose }
  ';'         { TkSemi }
  ':'         { TkColon }
  ','         { TkComma }
  '('         { TkLParen }
  ')'         { TkRParen }
  '+'         { TkPlus }
  '-'         { TkMinus }
  '*'         { TkStar }
  '/'         { TkSlash }
  '<'         { TkLT }
  '>'         { TkGT }
  '<='        { TkLEQ }
  '>='        { TkGEQ }
  '\\'        { TkBackslash }
  '->'        { TkArrow }
  vopen       { TkVOpen }
  vsemi       { TkVSemi }
  vclose      { TkVClose }

%%

-- Program is a list of top-level expressions or definitions
Program :: { Expr }
  : TopLevel                               { $1 }
  | vopen TopLevel vclose vsemi            { $2 }

TopLevel :: { Expr }
  : TopLevelExprs                          { wrapExpressions $1 }

TopLevelExprs :: { [ExprBodyExpr] }
  : TopLevelExpr                           { [$1] }
  | TopLevelExpr ';' TopLevelExprs         { $1 : $3 }
  | TopLevelExpr vsemi TopLevelExprs       { $1 : $3 }
  | vopen TopLevelExprs vclose             { $2 }
  | {- empty -}                            { [] }

TopLevelExpr :: { ExprBodyExpr }
  : let vopen LetDecls vclose            { combineDecls $3 }
  | let LetDecl                          { $2 }
  | MacroDef                             { $1 }
  | FuncDef                              { $1 }
  | Expr                                 { Expr $1 }

LetDecls :: { [ExprBodyExpr] }
  : LetDecl                             { [$1] }
  | LetDecl vsemi LetDecls              { $1 : $3 }
  | LetDecl ';' LetDecls                { $1 : $3 }
  | {- empty -}                         { [] }

LetDecl :: { ExprBodyExpr }
  : ident '=' Expr                      { Def (T.pack $1) $3 }

-- Macro definition
MacroDef :: { ExprBodyExpr }
  : macro ident '(' ParamList ')' ':' Expr { makeMacroDef $2 $4 $7 }

-- Function definitions
FuncDef :: { ExprBodyExpr }
  : fn ident '(' ParamList ')' ':' Expr    { makeFuncDef $2 $4 $7 }

ParamList :: { [T.Text] }
  : {- empty -}                            { [] }
  | ident                                  { [T.pack $1] }
  | ident ',' ParamList                    { T.pack $1 : $3 }

-- Expressions
Expr :: { Expr }
  : IfExpr                                 { $1 }
  | LambdaExpr                             { $1 }
  | ApplyExpr                              { $1 }
  | AddExpr                                { $1 }

-- If expressions with various forms
IfExpr :: { Expr }
  : if Expr ':' Expr                       { EIf $2 $4 (ELit LNil) }
  | if Expr ':' Expr else ':' Expr         { EIf $2 $4 $7 }

-- Lambda expressions
LambdaExpr :: { Expr }
  : '(' fn '(' ParamList ')' ':' Expr ')'  { ELam $4 (ExprBody [] $7) }

-- Function application
ApplyExpr :: { Expr }
  : Term                                   { $1 }
  | ApplyExpr '(' ExprList ')'             { EApp $1 $3 }
  | ApplyExpr Term                         { EApp $1 [$2] }

-- Addition and subtraction
AddExpr :: { Expr }
  : MulExpr                                { $1 }
  | AddExpr '+' MulExpr                    { EApp (EBuiltinIdent (T.pack "+")) [$1, $3] }
  | AddExpr '-' MulExpr                    { EApp (EBuiltinIdent (T.pack "-")) [$1, $3] }

-- Multiplication and division
MulExpr :: { Expr }
  : Term                                   { $1 }
  | MulExpr '*' Term                       { EApp (EBuiltinIdent (T.pack "*")) [$1, $3] }
  | MulExpr '/' Term                       { EApp (EBuiltinIdent (T.pack "/")) [$1, $3] }

ExprList :: { [Expr] }
  : {- empty -}                            { [] }
  | Expr                                   { [$1] }
  | Expr ',' ExprList                      { $1 : $3 }

-- Basic terms
Term :: { Expr }
  : ident                                  { makeIdentifier $1 }
  | intlit                                 { ELit (LInt $1) }
  | strlit                                 { ELit (LString (T.pack $1)) }
  | true                                   { ELit (LBool True) }
  | false                                  { ELit (LBool False) }
  | nil                                    { ELit LNil }
  | '(' Expr ')'                           { $2 }

-- Block of code (for function bodies, etc.)
Block :: { Expr }
  : Expr                                   { $1 }
  | vopen BlockExprs vclose                { makeBlock $2 }

BlockExprs :: { [ExprBodyExpr] }
  : BlockExpr                              { [$1] }
  | BlockExpr vsemi BlockExprs             { $1 : $3 }
  | {- empty -}                            { [] }

BlockExpr :: { ExprBodyExpr }
  : let ident '=' Expr                     { Def (T.pack $2) $4 }
  | Expr                                   { Expr $1 }

{

-- | Parse action monad with debug flag
type ParserM a = ReaderT Bool (LoggingT IO) a

-- | Run the lexer to get the next token
lexer :: (Token -> Lexer a) -> Lexer a
lexer cont = scan >>= cont

-- | Parse error handling
parseError :: Token -> Lexer a
parseError token = do
  throwError $ "Parse error on token: " ++ show token

-- Helper to determine if an identifier is a builtin function
isBuiltin :: String -> Bool
isBuiltin ident = ident `elem` [
  "display", "cons", "car", "cdr", "tostring",
  "+", "-", "*", "/", "<", ">", "<=", ">=", "eq?",
  "not", "cons?", "null?", "string-concat"
  ]

-- Helper to create identifier (variable or builtin)
makeIdentifier :: String -> Expr
makeIdentifier ident
  | isBuiltin ident = EBuiltinIdent (T.pack ident)
  | otherwise       = EVar (T.pack ident)

-- Helper to create a function definition
makeFuncDef :: String -> [T.Text] -> Expr -> ExprBodyExpr
makeFuncDef name params body =
  Def (T.pack name) (ELam params (ExprBody [] body))

-- Helper to create a macro definition
makeMacroDef :: String -> [T.Text] -> Expr -> ExprBodyExpr
makeMacroDef name params body =
  let macroParamNames = map (\i -> T.pack ("$" ++ show i)) [1 .. length params]
      macroImpl = ELam macroParamNames (ExprBody [] body)
      macroLambda = ELam [T.pack name] (ExprBody [] macroImpl)
  in Def (T.pack "macro") macroLambda

-- Helper to create a block of expressions
makeBlock :: [ExprBodyExpr] -> Expr
makeBlock [] = ELit LNil
makeBlock exprs =
  let (defs, bodyExprs) = partitionExprs exprs
      finalExpr = case bodyExprs of
                    [] -> ELit LNil
                    _  -> last bodyExprs
  in EApp (ELam [] (ExprBody defs finalExpr)) []

-- Helper to partition expressions into definitions and expressions
partitionExprs :: [ExprBodyExpr] -> ([ExprBodyExpr], [Expr])
partitionExprs = foldr categorize ([], [])
  where
    categorize (Def n e) (defs, exprs) = (Def n e : defs, exprs)
    categorize (Expr e) (defs, exprs) = (defs, e : exprs)

-- Wrap top-level expressions in a lambda application
wrapExpressions :: [ExprBodyExpr] -> Expr
wrapExpressions [] = ELit LNil
wrapExpressions exprs =
  let (defs, bodyExprs) = partitionExprs exprs
      finalExpr = case bodyExprs of
                    [] -> ELit LNil
                    _  -> last bodyExprs
  in trace ("Wrapping expressions!\n  exprs: " ++ show exprs ++ "\n  defs: " ++ show defs ++ "\n  bodyExprs: " ++ show bodyExprs) $ EApp (ELam [] (ExprBody defs finalExpr)) []

-- Partition the declarations into definitions and expressions.
partitionDecls :: [ExprBodyExpr] -> ([(T.Text, Expr)], [Expr])
partitionDecls [] = ([], [])
partitionDecls ((Def n e) : rest) =
  let (defs, exprs) = partitionDecls rest
  in ((n, e) : defs, exprs)
partitionDecls ((Expr e) : rest) =
  let (defs, exprs) = partitionDecls rest
  in (defs, e : exprs)

-- combineDecls takes a list of declarations from a block let and combines them
-- into a single let expression.
combineDecls :: [ExprBodyExpr] -> ExprBodyExpr
combineDecls decls = trace "Combining Decls!" $ Expr (ELet defs (ExprBody extraExprs finalExpr))
  where
    (defs, exprs) = partitionDecls decls
    -- If there are any expressions, take the last one as the final expression,
    -- and wrap any preceding ones with the Expr constructor.
    (extraExprs, finalExpr) = case reverse exprs of
      []           -> ([], ELit LNil)
      (final:rs)   -> (map Expr (reverse rs), final)

-- | Parse a program with debug information
parseWithDebug :: Bool -> T.Text -> IO (Either String Expr)
parseWithDebug debug input =
  -- Simply return the Either result directly in the IO monad
  return $ runLexer parseTopLevel (T.unpack input)

-- | Parse a program
parseProgram :: T.Text -> IO (Either String Expr)
parseProgram = parseWithDebug False
}
