{-# OPTIONS_GHC -w #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoStrictData #-}
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE PartialTypeSignatures #-}
#endif
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
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import qualified System.IO as Happy_System_IO
import qualified System.IO.Unsafe as Happy_System_IO_Unsafe
import qualified Debug.Trace as Happy_Debug_Trace
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 2.0.2

data HappyAbsSyn 
        = HappyTerminal (Token)
        | HappyErrorToken Prelude.Int
        | HappyAbsSyn5 (Expr)
        | HappyAbsSyn7 ([ExprBodyExpr])
        | HappyAbsSyn8 (ExprBodyExpr)
        | HappyAbsSyn13 ([T.Text])
        | HappyAbsSyn20 ([Expr])

happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x47\x0e\x10\x00\x00\x00\x00\xf0\xf4\x02\x01\x10\x00\x00\x00\x4f\x2f\x10\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x70\xe0\x00\x01\x00\x00\x00\x00\x00\x00\xc0\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x70\xe4\x00\x01\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x70\xf4\x00\x01\x00\x00\x00\x00\x4f\x2f\x10\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x2f\x10\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x70\xe0\x00\x01\x00\x00\x00\x00\x07\x0e\x10\x00\x00\x00\x00\x70\xe0\x00\x01\x00\x00\x00\x00\x07\x0e\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x47\x0e\x10\x00\x00\x00\x00\xf0\xf4\x02\x01\x10\x00\x00\x00\x4f\x2f\x10\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x70\xe4\x00\x01\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x02\x00\x02\x00\x00\x70\xe4\x00\x01\x00\x00\x00\x00\x47\x0e\x10\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x47\x0e\x10\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x47\x0e\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x47\x0e\x10\x00\x00\x00\x00\x70\xe4\x00\x01\x00\x00\x00\x00\x47\x0e\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseExpr","%start_parseTopLevel","Program","TopLevel","TopLevelExprs","TopLevelExpr","LetDecls","LetDecl","MacroDef","FuncDef","ParamList","Expr","IfExpr","LambdaExpr","ApplyExpr","AddExpr","MulExpr","ExprList","Term","Block","BlockExprs","BlockExpr","ident","intlit","strlit","let","in","where","if","else","fn","true","false","nil","not","macro","'='","'{'","'}'","';'","':'","','","'('","')'","'+'","'-'","'*'","'/'","'<'","'>'","'<='","'>='","'\\\\'","'->'","vopen","vsemi","vclose","%eof"]
        bit_start = st               Prelude.* 60
        bit_end   = (st Prelude.+ 1) Prelude.* 60
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..59]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x3f\x00\x00\x00\x01\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfe\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x42\x00\x00\x00\x48\x00\x00\x00\x56\x00\x00\x00\x15\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x3f\x00\x00\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x14\x00\x00\x00\x33\x00\x00\x00\x10\x00\x00\x00\xdd\xff\xff\xff\x10\x00\x00\x00\xdd\xff\xff\xff\xf5\xff\xff\xff\xfc\xff\xff\xff\xfc\xff\xff\xff\x0b\x00\x00\x00\x0f\x00\x00\x00\x26\x00\x00\x00\x32\x00\x00\x00\x17\x00\x00\x00\x00\x00\x00\x00\x43\x00\x00\x00\x55\x00\x00\x00\x4e\x00\x00\x00\x4e\x00\x00\x00\x4e\x00\x00\x00\x4e\x00\x00\x00\x00\x00\x00\x00\x3f\x00\x00\x00\x10\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf3\xff\xff\xff\x54\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x3f\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x11\x00\x00\x00\x3f\x00\x00\x00\x3f\x00\x00\x00\x79\x00\x00\x00\x79\x00\x00\x00\x79\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5d\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x00\x00\x6f\x00\x00\x00\x74\x00\x00\x00\x78\x00\x00\x00\x87\x00\x00\x00\x00\x00\x00\x00\x91\x00\x00\x00\x91\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3f\x00\x00\x00\x85\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8b\x00\x00\x00\x8e\x00\x00\x00\x95\x00\x00\x00\x9c\x00\x00\x00\x9a\x00\x00\x00\x3f\x00\x00\x00\x00\x00\x00\x00\x3f\x00\x00\x00\x3f\x00\x00\x00\x3f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x96\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xc0\x00\x00\x00\x5b\x00\x00\x00\x6b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb2\x00\x00\x00\xc8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd0\x00\x00\x00\x8a\x00\x00\x00\x00\x00\x00\x00\x7b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x01\x00\x00\xbf\x00\x00\x00\xc7\x00\x00\x00\x22\x00\x00\x00\x45\x00\x00\x00\x00\x00\x00\x00\xb0\x00\x00\x00\x99\x00\x00\x00\xa8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd8\x00\x00\x00\xe0\x00\x00\x00\xd7\x00\x00\x00\xdf\x00\x00\x00\xe7\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0f\x01\x00\x00\x11\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xef\x00\x00\x00\x00\x00\x00\x00\xe8\x00\x00\x00\x00\x00\x00\x00\xf0\x00\x00\x00\xf8\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\xf6\xff\xff\xff\x00\x00\x00\x00\xfd\xff\xff\xff\xfb\xff\xff\xff\xfa\xff\xff\xff\xf3\xff\xff\xff\xf2\xff\xff\xff\xf1\xff\xff\xff\xe6\xff\xff\xff\xe5\xff\xff\xff\xe4\xff\xff\xff\xe3\xff\xff\xff\xdc\xff\xff\xff\xdf\xff\xff\xff\xd3\xff\xff\xff\xd2\xff\xff\xff\xd1\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd0\xff\xff\xff\xcf\xff\xff\xff\xce\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf6\xff\xff\xff\x00\x00\x00\x00\xf6\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf4\xff\xff\xff\x00\x00\x00\x00\xed\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdd\xff\xff\xff\xd6\xff\xff\xff\xf6\xff\xff\xff\xf6\xff\xff\xff\xf8\xff\xff\xff\xf9\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xda\xff\xff\xff\xd9\xff\xff\xff\x00\x00\x00\x00\xdb\xff\xff\xff\xd7\xff\xff\xff\xd8\xff\xff\xff\x00\x00\x00\x00\xf0\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe9\xff\xff\xff\xe9\xff\xff\xff\xe9\xff\xff\xff\xcd\xff\xff\xff\xf7\xff\xff\xff\x00\x00\x00\x00\xfc\xff\xff\xff\x00\x00\x00\x00\xe8\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe2\xff\xff\xff\xec\xff\xff\xff\xed\xff\xff\xff\xed\xff\xff\xff\xf5\xff\xff\xff\xde\xff\xff\xff\xd6\xff\xff\xff\xd5\xff\xff\xff\xd4\xff\xff\xff\xef\xff\xff\xff\xee\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe9\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe7\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe1\xff\xff\xff\xea\xff\xff\xff\xeb\xff\xff\xff\x00\x00\x00\x00\xe0\xff\xff\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\xff\xff\x24\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x01\x00\x00\x00\x14\x00\x00\x00\x07\x00\x00\x00\x16\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x01\x00\x00\x00\x0e\x00\x00\x00\x12\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x01\x00\x00\x00\x15\x00\x00\x00\x07\x00\x00\x00\x23\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x08\x00\x00\x00\x0e\x00\x00\x00\x23\x00\x00\x00\x22\x00\x00\x00\x16\x00\x00\x00\x21\x00\x00\x00\x12\x00\x00\x00\x15\x00\x00\x00\x15\x00\x00\x00\x21\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x13\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x0e\x00\x00\x00\x21\x00\x00\x00\x10\x00\x00\x00\x22\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x07\x00\x00\x00\x15\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x07\x00\x00\x00\x15\x00\x00\x00\x15\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x0f\x00\x00\x00\x0e\x00\x00\x00\x15\x00\x00\x00\x10\x00\x00\x00\x01\x00\x00\x00\x15\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x15\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x16\x00\x00\x00\x10\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x23\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x01\x00\x00\x00\x10\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x22\x00\x00\x00\x16\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x14\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x16\x00\x00\x00\x10\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x16\x00\x00\x00\x08\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x01\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x14\x00\x00\x00\x10\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x13\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x13\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x13\x00\x00\x00\x10\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x16\x00\x00\x00\x13\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x10\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x05\x00\x00\x00\x10\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x10\x00\x00\x00\x10\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x10\x00\x00\x00\x10\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x08\x00\x00\x00\x10\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x08\x00\x00\x00\x10\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x08\x00\x00\x00\x10\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x08\x00\x00\x00\x10\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\xff\xff\xff\xff\x10\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\xff\xff\xff\xff\x10\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\xff\xff\xff\xff\x10\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x00\x00\xff\xff\xff\xff\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x28\x00\x00\x00\x51\x00\x00\x00\x14\x00\x00\x00\x43\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x25\x00\x00\x00\x19\x00\x00\x00\x30\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x24\x00\x00\x00\x1a\x00\x00\x00\x14\x00\x00\x00\x45\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\xd9\xff\xff\xff\x19\x00\x00\x00\x44\x00\x00\x00\x31\x00\x00\x00\x43\x00\x00\x00\x1d\x00\x00\x00\x4d\x00\x00\x00\x42\x00\x00\x00\x1a\x00\x00\x00\x29\x00\x00\x00\xd9\xff\xff\xff\xd9\xff\xff\xff\xd9\xff\xff\xff\x3f\x00\x00\x00\xd9\xff\xff\xff\xd9\xff\xff\xff\xd9\xff\xff\xff\xd9\xff\xff\xff\xd9\xff\xff\xff\x38\x00\x00\x00\x1b\x00\x00\x00\x36\x00\x00\x00\x4e\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\xd9\xff\xff\xff\xd9\xff\xff\xff\xd9\xff\xff\xff\x14\x00\x00\x00\x41\x00\x00\x00\x23\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x14\x00\x00\x00\x40\x00\x00\x00\x1a\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x3e\x00\x00\x00\x35\x00\x00\x00\x1a\x00\x00\x00\x36\x00\x00\x00\x28\x00\x00\x00\x2f\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x1b\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x2c\x00\x00\x00\x2d\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x38\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x50\x00\x00\x00\x0e\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x2a\x00\x00\x00\x2b\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x4f\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x48\x00\x00\x00\x0e\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x05\x00\x00\x00\x46\x00\x00\x00\x5a\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x59\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x58\x00\x00\x00\x0e\x00\x00\x00\x20\x00\x00\x00\x05\x00\x00\x00\x57\x00\x00\x00\x56\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x28\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x51\x00\x00\x00\x0e\x00\x00\x00\x32\x00\x00\x00\x05\x00\x00\x00\x48\x00\x00\x00\x5f\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x5e\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x5d\x00\x00\x00\x0e\x00\x00\x00\x31\x00\x00\x00\x05\x00\x00\x00\x64\x00\x00\x00\x5b\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x2d\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x26\x00\x00\x00\x0e\x00\x00\x00\x33\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x34\x00\x00\x00\x0e\x00\x00\x00\x51\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x52\x00\x00\x00\x0e\x00\x00\x00\x1d\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x3a\x00\x00\x00\x0e\x00\x00\x00\x25\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x39\x00\x00\x00\x0e\x00\x00\x00\x21\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x49\x00\x00\x00\x0e\x00\x00\x00\x4b\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x48\x00\x00\x00\x0e\x00\x00\x00\x4a\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x46\x00\x00\x00\x0e\x00\x00\x00\x62\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x5b\x00\x00\x00\x0e\x00\x00\x00\x61\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x00\x00\x60\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x00\x00\x5f\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x00\x00\x3b\x00\x00\x00\x3c\x00\x00\x00\x54\x00\x00\x00\x3c\x00\x00\x00\x53\x00\x00\x00\x3c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (2, 57) [
        (2 , happyReduce_2),
        (3 , happyReduce_3),
        (4 , happyReduce_4),
        (5 , happyReduce_5),
        (6 , happyReduce_6),
        (7 , happyReduce_7),
        (8 , happyReduce_8),
        (9 , happyReduce_9),
        (10 , happyReduce_10),
        (11 , happyReduce_11),
        (12 , happyReduce_12),
        (13 , happyReduce_13),
        (14 , happyReduce_14),
        (15 , happyReduce_15),
        (16 , happyReduce_16),
        (17 , happyReduce_17),
        (18 , happyReduce_18),
        (19 , happyReduce_19),
        (20 , happyReduce_20),
        (21 , happyReduce_21),
        (22 , happyReduce_22),
        (23 , happyReduce_23),
        (24 , happyReduce_24),
        (25 , happyReduce_25),
        (26 , happyReduce_26),
        (27 , happyReduce_27),
        (28 , happyReduce_28),
        (29 , happyReduce_29),
        (30 , happyReduce_30),
        (31 , happyReduce_31),
        (32 , happyReduce_32),
        (33 , happyReduce_33),
        (34 , happyReduce_34),
        (35 , happyReduce_35),
        (36 , happyReduce_36),
        (37 , happyReduce_37),
        (38 , happyReduce_38),
        (39 , happyReduce_39),
        (40 , happyReduce_40),
        (41 , happyReduce_41),
        (42 , happyReduce_42),
        (43 , happyReduce_43),
        (44 , happyReduce_44),
        (45 , happyReduce_45),
        (46 , happyReduce_46),
        (47 , happyReduce_47),
        (48 , happyReduce_48),
        (49 , happyReduce_49),
        (50 , happyReduce_50),
        (51 , happyReduce_51),
        (52 , happyReduce_52),
        (53 , happyReduce_53),
        (54 , happyReduce_54),
        (55 , happyReduce_55),
        (56 , happyReduce_56),
        (57 , happyReduce_57)
        ]

happy_n_terms = 37 :: Prelude.Int
happy_n_nonterms = 20 :: Prelude.Int

happyReduce_2 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_2 = happySpecReduce_1  0# happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn5
                 (happy_var_1
        )
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_3 = happyReduce 4# 0# happyReduction_3
happyReduction_3 (_ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn5  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn5
                 (happy_var_2
        ) `HappyStk` happyRest

happyReduce_4 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_4 = happySpecReduce_1  1# happyReduction_4
happyReduction_4 (HappyAbsSyn7  happy_var_1)
         =  HappyAbsSyn5
                 (wrapExpressions happy_var_1
        )
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_5 = happySpecReduce_1  2# happyReduction_5
happyReduction_5 (HappyAbsSyn8  happy_var_1)
         =  HappyAbsSyn7
                 ([happy_var_1]
        )
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_6 = happySpecReduce_3  2# happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_3)
        _
        (HappyAbsSyn8  happy_var_1)
         =  HappyAbsSyn7
                 (happy_var_1 : happy_var_3
        )
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_7 = happySpecReduce_3  2# happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_3)
        _
        (HappyAbsSyn8  happy_var_1)
         =  HappyAbsSyn7
                 (happy_var_1 : happy_var_3
        )
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_8 = happySpecReduce_3  2# happyReduction_8
happyReduction_8 _
        (HappyAbsSyn7  happy_var_2)
        _
         =  HappyAbsSyn7
                 (happy_var_2
        )
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_9 = happySpecReduce_0  2# happyReduction_9
happyReduction_9  =  HappyAbsSyn7
                 ([]
        )

happyReduce_10 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_10 = happyReduce 4# 3# happyReduction_10
happyReduction_10 (_ `HappyStk`
        (HappyAbsSyn7  happy_var_3) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn8
                 (combineDecls happy_var_3
        ) `HappyStk` happyRest

happyReduce_11 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_11 = happySpecReduce_2  3# happyReduction_11
happyReduction_11 (HappyAbsSyn8  happy_var_2)
        _
         =  HappyAbsSyn8
                 (happy_var_2
        )
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_12 = happySpecReduce_1  3# happyReduction_12
happyReduction_12 (HappyAbsSyn8  happy_var_1)
         =  HappyAbsSyn8
                 (happy_var_1
        )
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_13 = happySpecReduce_1  3# happyReduction_13
happyReduction_13 (HappyAbsSyn8  happy_var_1)
         =  HappyAbsSyn8
                 (happy_var_1
        )
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_14 = happySpecReduce_1  3# happyReduction_14
happyReduction_14 (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn8
                 (Expr happy_var_1
        )
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_15 = happySpecReduce_1  4# happyReduction_15
happyReduction_15 (HappyAbsSyn8  happy_var_1)
         =  HappyAbsSyn7
                 ([happy_var_1]
        )
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_16 = happySpecReduce_3  4# happyReduction_16
happyReduction_16 (HappyAbsSyn7  happy_var_3)
        _
        (HappyAbsSyn8  happy_var_1)
         =  HappyAbsSyn7
                 (happy_var_1 : happy_var_3
        )
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_17 = happySpecReduce_3  4# happyReduction_17
happyReduction_17 (HappyAbsSyn7  happy_var_3)
        _
        (HappyAbsSyn8  happy_var_1)
         =  HappyAbsSyn7
                 (happy_var_1 : happy_var_3
        )
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_18 = happySpecReduce_0  4# happyReduction_18
happyReduction_18  =  HappyAbsSyn7
                 ([]
        )

happyReduce_19 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_19 = happySpecReduce_3  5# happyReduction_19
happyReduction_19 (HappyAbsSyn5  happy_var_3)
        _
        (HappyTerminal (TkIdent happy_var_1))
         =  HappyAbsSyn8
                 (Def (T.pack happy_var_1) happy_var_3
        )
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_20 = happyReduce 7# 6# happyReduction_20
happyReduction_20 ((HappyAbsSyn5  happy_var_7) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn13  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyTerminal (TkIdent happy_var_2)) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn8
                 (makeMacroDef happy_var_2 happy_var_4 happy_var_7
        ) `HappyStk` happyRest

happyReduce_21 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_21 = happyReduce 7# 7# happyReduction_21
happyReduction_21 ((HappyAbsSyn5  happy_var_7) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn13  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyTerminal (TkIdent happy_var_2)) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn8
                 (makeFuncDef happy_var_2 happy_var_4 happy_var_7
        ) `HappyStk` happyRest

happyReduce_22 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_22 = happySpecReduce_0  8# happyReduction_22
happyReduction_22  =  HappyAbsSyn13
                 ([]
        )

happyReduce_23 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_23 = happySpecReduce_1  8# happyReduction_23
happyReduction_23 (HappyTerminal (TkIdent happy_var_1))
         =  HappyAbsSyn13
                 ([T.pack happy_var_1]
        )
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_24 = happySpecReduce_3  8# happyReduction_24
happyReduction_24 (HappyAbsSyn13  happy_var_3)
        _
        (HappyTerminal (TkIdent happy_var_1))
         =  HappyAbsSyn13
                 (T.pack happy_var_1 : happy_var_3
        )
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_25 = happySpecReduce_1  9# happyReduction_25
happyReduction_25 (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn5
                 (happy_var_1
        )
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_26 = happySpecReduce_1  9# happyReduction_26
happyReduction_26 (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn5
                 (happy_var_1
        )
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_27 = happySpecReduce_1  9# happyReduction_27
happyReduction_27 (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn5
                 (happy_var_1
        )
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_28 = happySpecReduce_1  9# happyReduction_28
happyReduction_28 (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn5
                 (happy_var_1
        )
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_29 = happyReduce 4# 10# happyReduction_29
happyReduction_29 ((HappyAbsSyn5  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn5  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn5
                 (EIf happy_var_2 happy_var_4 (ELit LNil)
        ) `HappyStk` happyRest

happyReduce_30 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_30 = happyReduce 7# 10# happyReduction_30
happyReduction_30 ((HappyAbsSyn5  happy_var_7) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn5  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn5  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn5
                 (EIf happy_var_2 happy_var_4 happy_var_7
        ) `HappyStk` happyRest

happyReduce_31 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_31 = happyReduce 8# 11# happyReduction_31
happyReduction_31 (_ `HappyStk`
        (HappyAbsSyn5  happy_var_7) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn13  happy_var_4) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn5
                 (ELam happy_var_4 (ExprBody [] happy_var_7)
        ) `HappyStk` happyRest

happyReduce_32 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_32 = happySpecReduce_1  12# happyReduction_32
happyReduction_32 (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn5
                 (happy_var_1
        )
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_33 = happyReduce 4# 12# happyReduction_33
happyReduction_33 (_ `HappyStk`
        (HappyAbsSyn20  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn5  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn5
                 (EApp happy_var_1 happy_var_3
        ) `HappyStk` happyRest

happyReduce_34 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_34 = happySpecReduce_2  12# happyReduction_34
happyReduction_34 (HappyAbsSyn5  happy_var_2)
        (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn5
                 (EApp happy_var_1 [happy_var_2]
        )
happyReduction_34 _ _  = notHappyAtAll 

happyReduce_35 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_35 = happySpecReduce_1  13# happyReduction_35
happyReduction_35 (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn5
                 (happy_var_1
        )
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_36 = happySpecReduce_3  13# happyReduction_36
happyReduction_36 (HappyAbsSyn5  happy_var_3)
        _
        (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn5
                 (EApp (EBuiltinIdent (T.pack "+")) [happy_var_1, happy_var_3]
        )
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_37 = happySpecReduce_3  13# happyReduction_37
happyReduction_37 (HappyAbsSyn5  happy_var_3)
        _
        (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn5
                 (EApp (EBuiltinIdent (T.pack "-")) [happy_var_1, happy_var_3]
        )
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_38 = happySpecReduce_1  14# happyReduction_38
happyReduction_38 (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn5
                 (happy_var_1
        )
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_39 = happySpecReduce_3  14# happyReduction_39
happyReduction_39 (HappyAbsSyn5  happy_var_3)
        _
        (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn5
                 (EApp (EBuiltinIdent (T.pack "*")) [happy_var_1, happy_var_3]
        )
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_40 = happySpecReduce_3  14# happyReduction_40
happyReduction_40 (HappyAbsSyn5  happy_var_3)
        _
        (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn5
                 (EApp (EBuiltinIdent (T.pack "/")) [happy_var_1, happy_var_3]
        )
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_41 = happySpecReduce_0  15# happyReduction_41
happyReduction_41  =  HappyAbsSyn20
                 ([]
        )

happyReduce_42 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_42 = happySpecReduce_1  15# happyReduction_42
happyReduction_42 (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn20
                 ([happy_var_1]
        )
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_43 = happySpecReduce_3  15# happyReduction_43
happyReduction_43 (HappyAbsSyn20  happy_var_3)
        _
        (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn20
                 (happy_var_1 : happy_var_3
        )
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_44 = happySpecReduce_1  16# happyReduction_44
happyReduction_44 (HappyTerminal (TkIdent happy_var_1))
         =  HappyAbsSyn5
                 (makeIdentifier happy_var_1
        )
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_45 = happySpecReduce_1  16# happyReduction_45
happyReduction_45 (HappyTerminal (TkIntLit happy_var_1))
         =  HappyAbsSyn5
                 (ELit (LInt happy_var_1)
        )
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_46 = happySpecReduce_1  16# happyReduction_46
happyReduction_46 (HappyTerminal (TkStringLit happy_var_1))
         =  HappyAbsSyn5
                 (ELit (LString (T.pack happy_var_1))
        )
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_47 = happySpecReduce_1  16# happyReduction_47
happyReduction_47 _
         =  HappyAbsSyn5
                 (ELit (LBool True)
        )

happyReduce_48 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_48 = happySpecReduce_1  16# happyReduction_48
happyReduction_48 _
         =  HappyAbsSyn5
                 (ELit (LBool False)
        )

happyReduce_49 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_49 = happySpecReduce_1  16# happyReduction_49
happyReduction_49 _
         =  HappyAbsSyn5
                 (ELit LNil
        )

happyReduce_50 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_50 = happySpecReduce_3  16# happyReduction_50
happyReduction_50 _
        (HappyAbsSyn5  happy_var_2)
        _
         =  HappyAbsSyn5
                 (happy_var_2
        )
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_51 = happySpecReduce_1  17# happyReduction_51
happyReduction_51 (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn5
                 (happy_var_1
        )
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_52 = happySpecReduce_3  17# happyReduction_52
happyReduction_52 _
        (HappyAbsSyn7  happy_var_2)
        _
         =  HappyAbsSyn5
                 (makeBlock happy_var_2
        )
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_53 = happySpecReduce_1  18# happyReduction_53
happyReduction_53 (HappyAbsSyn8  happy_var_1)
         =  HappyAbsSyn7
                 ([happy_var_1]
        )
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_54 = happySpecReduce_3  18# happyReduction_54
happyReduction_54 (HappyAbsSyn7  happy_var_3)
        _
        (HappyAbsSyn8  happy_var_1)
         =  HappyAbsSyn7
                 (happy_var_1 : happy_var_3
        )
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_55 = happySpecReduce_0  18# happyReduction_55
happyReduction_55  =  HappyAbsSyn7
                 ([]
        )

happyReduce_56 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_56 = happyReduce 4# 19# happyReduction_56
happyReduction_56 ((HappyAbsSyn5  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyTerminal (TkIdent happy_var_2)) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn8
                 (Def (T.pack happy_var_2) happy_var_4
        ) `HappyStk` happyRest

happyReduce_57 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )
happyReduce_57 = happySpecReduce_1  19# happyReduction_57
happyReduction_57 (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn8
                 (Expr happy_var_1
        )
happyReduction_57 _  = notHappyAtAll 

happyNewToken action sts stk
        = lexer(\tk -> 
	let cont i = happyDoAction i tk action sts stk in
        case tk of {
        TkEOF -> happyDoAction 36# tk action sts stk;
        TkIdent happy_dollar_dollar -> cont 1#;
        TkIntLit happy_dollar_dollar -> cont 2#;
        TkStringLit happy_dollar_dollar -> cont 3#;
        TkLet -> cont 4#;
        TkIn -> cont 5#;
        TkWhere -> cont 6#;
        TkIf -> cont 7#;
        TkElse -> cont 8#;
        TkFn -> cont 9#;
        TkTrue -> cont 10#;
        TkFalse -> cont 11#;
        TkNil -> cont 12#;
        TkNot -> cont 13#;
        TkMacro -> cont 14#;
        TkEqual -> cont 15#;
        TkOpen -> cont 16#;
        TkClose -> cont 17#;
        TkSemi -> cont 18#;
        TkColon -> cont 19#;
        TkComma -> cont 20#;
        TkLParen -> cont 21#;
        TkRParen -> cont 22#;
        TkPlus -> cont 23#;
        TkMinus -> cont 24#;
        TkStar -> cont 25#;
        TkSlash -> cont 26#;
        TkLT -> cont 27#;
        TkGT -> cont 28#;
        TkLEQ -> cont 29#;
        TkGEQ -> cont 30#;
        TkBackslash -> cont 31#;
        TkArrow -> cont 32#;
        TkVOpen -> cont 33#;
        TkVSemi -> cont 34#;
        TkVClose -> cont 35#;
        _ -> happyError' (tk, [])
        })

happyError_ explist 36# tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => Lexer a -> (a -> Lexer b) -> Lexer b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> Lexer a
happyReturn = (Prelude.return)
happyParse :: () => Happy_GHC_Exts.Int# -> Lexer (HappyAbsSyn )

happyNewToken :: () => Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )

happyDoAction :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn )

happyReduceArr :: () => Happy_Data_Array.Array Prelude.Int (Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Lexer (HappyAbsSyn ))

happyThen1 :: () => Lexer a -> (a -> Lexer b) -> Lexer b
happyThen1 = happyThen
happyReturn1 :: () => a -> Lexer a
happyReturn1 = happyReturn
happyError' :: () => ((Token), [Prelude.String]) -> Lexer a
happyError' tk = (\(tokens, _) -> parseError tokens) tk
parseExpr = happySomeParser where
 happySomeParser = happyThen (happyParse 0#) (\x -> case x of {HappyAbsSyn5 z -> happyReturn z; _other -> notHappyAtAll })

parseTopLevel = happySomeParser where
 happySomeParser = happyThen (happyParse 1#) (\x -> case x of {HappyAbsSyn5 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
#define HAPPY_DEBUG 1
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $

#if !defined(__GLASGOW_HASKELL__)
#  error This code isn't being built with GHC.
#endif

-- Get WORDS_BIGENDIAN (if defined)
#include "MachDeps.h"

-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#  define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#  define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#  define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#else
#  define LT(n,m) (n Happy_GHC_Exts.<# m)
#  define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#  define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
#define PLUS(n,m) (n Happy_GHC_Exts.+# m)
#define MINUS(n,m) (n Happy_GHC_Exts.-# m)
#define TIMES(n,m) (n Happy_GHC_Exts.*# m)
#define NEGATE(n) (Happy_GHC_Exts.negateInt# (n))

type Happy_Int = Happy_GHC_Exts.Int#
data Happy_IntList = HappyCons Happy_Int Happy_IntList

#define ERROR_TOK 0#

#if defined(HAPPY_COERCE)
#  define GET_ERROR_TOKEN(x)  (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (happyInTok (x))
#else
#  define GET_ERROR_TOKEN(x)  (case x of { HappyErrorToken (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (HappyErrorToken (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (HappyTerminal (x))
#endif

#if defined(HAPPY_DEBUG)
#  define DEBUG_TRACE(s)    (happyTrace (s)) $
happyTrace string expr = Happy_System_IO_Unsafe.unsafePerformIO $ do
    Happy_System_IO.hPutStr Happy_System_IO.stderr string
    return expr
#else
#  define DEBUG_TRACE(s)    {- nothing -}
#endif

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept ERROR_TOK tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) =
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

happyDoAction i tk st =
  DEBUG_TRACE("state: " ++ show (Happy_GHC_Exts.I# st) ++
              ",\ttoken: " ++ show (Happy_GHC_Exts.I# i) ++
              ",\taction: ")
  case happyDecodeAction (happyNextAction i st) of
    HappyFail             -> DEBUG_TRACE("failing.\n")
                             happyFail (happyExpListPerState (Happy_GHC_Exts.I# st)) i tk st
    HappyAccept           -> DEBUG_TRACE("accept.\n")
                             happyAccept i tk st
    HappyReduce rule      -> DEBUG_TRACE("reduce (rule " ++ show (Happy_GHC_Exts.I# rule) ++ ")")
                             (happyReduceArr Happy_Data_Array.! (Happy_GHC_Exts.I# rule)) i tk st
    HappyShift  new_state -> DEBUG_TRACE("shift, enter state " ++ show (Happy_GHC_Exts.I# new_state) ++ "\n")
                             happyShift new_state i tk st

{-# INLINE happyNextAction #-}
happyNextAction i st = case happyIndexActionTable i st of
  Just (Happy_GHC_Exts.I# act) -> act
  Nothing                      -> happyIndexOffAddr happyDefActions st

{-# INLINE happyIndexActionTable #-}
happyIndexActionTable i st
  | GTE(off, 0#), EQ(happyIndexOffAddr happyCheck off, i)
  = Prelude.Just (Happy_GHC_Exts.I# (happyIndexOffAddr happyTable off))
  | otherwise
  = Prelude.Nothing
  where
    off = PLUS(happyIndexOffAddr happyActOffsets st, i)

data HappyAction
  = HappyFail
  | HappyAccept
  | HappyReduce Happy_Int -- rule number
  | HappyShift Happy_Int  -- new state

{-# INLINE happyDecodeAction #-}
happyDecodeAction :: Happy_Int -> HappyAction
happyDecodeAction  0#                        = HappyFail
happyDecodeAction -1#                        = HappyAccept
happyDecodeAction action | LT(action, 0#)    = HappyReduce NEGATE(PLUS(action, 1#))
                         | otherwise         = HappyShift MINUS(action, 1#)

{-# INLINE happyIndexGotoTable #-}
happyIndexGotoTable nt st = happyIndexOffAddr happyTable off
  where
    off = PLUS(happyIndexOffAddr happyGotoOffsets st, nt)

{-# INLINE happyIndexOffAddr #-}
happyIndexOffAddr :: HappyAddr -> Happy_Int -> Happy_Int
happyIndexOffAddr (HappyA# arr) off =
#if __GLASGOW_HASKELL__ >= 901
  Happy_GHC_Exts.int32ToInt# -- qualified import because it doesn't exist on older GHC's
#endif
#ifdef WORDS_BIGENDIAN
  -- The CI of `alex` tests this code path
  (Happy_GHC_Exts.word32ToInt32# (Happy_GHC_Exts.wordToWord32# (Happy_GHC_Exts.byteSwap32# (Happy_GHC_Exts.word32ToWord# (Happy_GHC_Exts.int32ToWord32#
#endif
  (Happy_GHC_Exts.indexInt32OffAddr# arr off)
#ifdef WORDS_BIGENDIAN
  )))))
#endif

{-# INLINE happyLt #-}
happyLt x y = LT(x,y)

readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (happyIndexOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 5#))) (bit `Prelude.mod` 32)
  where unbox_int (Happy_GHC_Exts.I# x) = x

data HappyAddr = HappyA# Happy_GHC_Exts.Addr#

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state ERROR_TOK tk st sts stk@(x `HappyStk` _) =
     let i = GET_ERROR_TOKEN(x) in
-- trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons st sts) stk

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons st sts) (MK_TOKEN(tk) `HappyStk` stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_0 nt fn j tk st sts stk
     = happyGoto nt j tk st (HappyCons st sts) (fn `HappyStk` stk)

happySpecReduce_1 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(HappyCons st _) (v1 `HappyStk` stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_2 nt fn j tk _
  (HappyCons _ sts@(HappyCons st _))
  (v1 `HappyStk` v2 `HappyStk` stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_3 nt fn j tk _
  (HappyCons _ (HappyCons _ sts@(HappyCons st _)))
  (v1 `HappyStk` v2 `HappyStk` v3 `HappyStk` stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop MINUS(k,(1# :: Happy_Int)) sts of
         sts1@(HappyCons st1 _) ->
                let r = fn stk in -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk)
                     (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk
              off = happyAdjustOffset (happyIndexOffAddr happyGotoOffsets st1)
              off_i = PLUS(off, nt)
              new_state = happyIndexOffAddr happyTable off_i
          in
            happyThen1 (fn stk tk)
                       (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l               = l
happyDrop n  (HappyCons _ t) = happyDrop MINUS(n,(1# :: Happy_Int)) t

happyDropStk 0# l                 = l
happyDropStk n  (x `HappyStk` xs) = happyDropStk MINUS(n,(1#::Happy_Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

happyGoto nt j tk st =
   DEBUG_TRACE(", goto state " ++ show (Happy_GHC_Exts.I# new_state) ++ "\n")
   happyDoAction j tk new_state
  where new_state = happyIndexGotoTable nt st

-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist ERROR_TOK tk old_st _ stk@(x `HappyStk` _) =
     let i = GET_ERROR_TOKEN(x) in
--      trace "failing" $
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st (HappyCons action sts)
                               (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction ERROR_TOK tk action sts (saved_tok`HappyStk`stk)
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk action sts stk =
-- trace "entering error recovery" $
        happyDoAction ERROR_TOK tk action sts (MK_ERROR_TOKEN(i) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions

happyTcHack :: Happy_Int -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}

-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
