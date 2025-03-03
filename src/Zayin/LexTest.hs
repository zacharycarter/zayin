-- |

module Zayin.LexTest (lexAll) where

import Debug.Trace (traceM)

import Zayin.Lexer
import Zayin.Lexer.Support

lexAll :: Lexer ()
lexAll = do
  tok <- scan
  case tok of
    TkEOF -> pure ()
    x -> do
      traceM (show x)
      lexAll
