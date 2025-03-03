{
module Zayin.Lexer where

import Control.Monad.State
import Control.Monad.Except
import Zayin.Lexer.Support

import Debug.Trace

}

%encoding "latin1"

$digit = [ 0-9 ]
$lower = [ a-z ]
$upper = [ A-Z ]

@ident = $lower [ $lower $upper _ ' ]*

:-

[\ \t]+ ;

$digit+ { emit (TkIntLit . read) }

<0> "in"    { token TkIn }
<0> "let"   { layoutKw TkLet }
<0> "where" { layoutKw TkWhere }

<0> @ident { emit TkIdent }
<0> \\     { token TkBackslash }
<0> "->"   { token TkArrow }
<0> \=     { token TkEqual }
<0> \(     { token TkLParen }
<0> \)     { token TkRParen }
<0> \{     { token TkOpen }
<0> \}     { token TkClose }


<0> "--" .* \n { \_ -> pushStartCode newline *> scan }
<0> \n         { \_ -> pushStartCode newline *> scan }

<layout> {
  -- Skip comments and whitespace
  "--" .* \n ;
  \n       ;

  \{ { openBrace }
  () { startLayout }
}

<empty_layout> () { emptyLayout }

<newline> {
  \n         ;
  "--" .* \n ;

  () { offsideRule }
}

<eof> () { doEOF }

{
handleEOF = pushStartCode eof *> scan

doEOF _ = do
  t <- Zayin.Lexer.Support.layout
  case t of
    Nothing -> do
      popStartCode
      pure TkEOF
    _ -> do
      popLayout
      pure TkVClose

scan :: Lexer Token
scan = do
  input@(Input _ _ _ string) <- gets lexerInput
  startcode <- startCode
  case alexScan input startcode of
    AlexEOF -> handleEOF
    AlexError (Input _ _ _ inp) ->
      throwError $ "Lexical error: " ++ show (head inp)
    AlexSkip input' _ -> do
      modify' $ \s -> s { lexerInput = input' }
      scan
    AlexToken input' tokl action -> do
      modify' $ \s -> s { lexerInput = input' }
      action (take tokl string)

layoutKw t _ = do
  pushStartCode Zayin.Lexer.layout
  pure t

openBrace _ = do
  popStartCode
  pushLayout ExplicitLayout
  pure TkOpen

startLayout _ = do
  popStartCode

  reference <- Zayin.Lexer.Support.layout
  col       <- gets (inpColumn . lexerInput)

  if Just (LayoutColumn col) <= reference
    then pushStartCode empty_layout
    else pushLayout (LayoutColumn col)

  pure TkVOpen

emptyLayout _ = do
  popStartCode
  pushStartCode newline
  pure TkVClose

offsideRule _ = do
  context <- Zayin.Lexer.Support.layout
  col <- gets (inpColumn . lexerInput)

  let continue = popStartCode *> scan

  case context of
    Just (LayoutColumn col') -> do
      case col `compare` col' of
        EQ -> do
          popStartCode
          pure TkVSemi
        GT -> continue
        LT -> do
          popLayout
          pure TkVClose
    _ -> continue
}
