{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Zayin.Lexer.Support
  ( AlexInput (..),
    Layout (..),
    Lexer,
    LexerState (..),
    Token (..),
    alexGetByte,
    alexInputPrevChar,
    emit,
    layout,
    popLayout,
    pushLayout,
    startCode,
    popStartCode,
    pushStartCode,
    token,
    runLexer,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Data.Char (ord)
import Data.List (uncons)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Word

-- | Tokens for the Zayin language
data Token
  = TkIdent String                 -- Identifiers
  | -- Keywords
    TkLet
  | TkIn
  | TkWhere
  | TkIf
  | TkElse
  | TkFn
  | TkTrue
  | TkFalse
  | TkNil
  | TkNot
  | TkMacro
  | -- Literals
    TkIntLit Integer
  | TkStringLit String
  | -- Punctuation
    TkEqual
  | TkOpen
  | TkSemi
  | TkClose
  | TkLParen
  | TkRParen
  | TkBackslash
  | TkArrow
  | TkColon
  | TkComma
  | -- Operators
    TkPlus
  | TkMinus
  | TkStar
  | TkSlash
  | TkLT
  | TkGT
  | TkLEQ
  | TkGEQ
  | -- Layout punctuation
    TkVOpen
  | TkVSemi
  | TkVClose
  | TkEOF
  deriving (Eq, Show)

-- | Alex input state
data AlexInput = Input
  { inpLine :: {-# UNPACK #-} !Int,
    inpColumn :: {-# UNPACK #-} !Int,
    inpLast :: {-# UNPACK #-} !Char,
    inpStream :: String
  }
  deriving (Eq, Show)

alexPrevInputChar :: AlexInput -> Char
alexPrevInputChar = inpLast

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = inpLast

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte inp@Input {inpStream = str} = advance <$> uncons str
  where
    advance ('\n', rest) =
      ( fromIntegral (ord '\n'),
        Input
          { inpLine = inpLine inp + 1,
            inpColumn = 0,
            inpLast = '\n',
            inpStream = rest
          }
      )
    advance (c, rest) =
      ( fromIntegral (ord c),
        Input
          { inpLine = inpLine inp,
            inpColumn = inpColumn inp + 1,
            inpLast = c,
            inpStream = rest
          }
      )

-- | Lexer monad
newtype Lexer a = Lexer {_getLexer :: StateT LexerState (Either String) a}
  deriving (Functor, Applicative, Monad, MonadState LexerState, MonadError String)

-- | Layout management data type
data Layout = ExplicitLayout | LayoutColumn Int
  deriving (Eq, Show, Ord)

-- | Lexer state
data LexerState = LS
  { lexerInput :: {-# UNPACK #-} !AlexInput,
    lexerStartCodes :: {-# UNPACK #-} !(NonEmpty Int),
    lexerLayout :: [Layout]
  }
  deriving (Eq, Show)

-- | Get the current start code
startCode :: Lexer Int
startCode = gets (NE.head . lexerStartCodes)

-- | Push a new start code onto the stack
pushStartCode :: Int -> Lexer ()
pushStartCode i = modify' $ \st ->
  st
    { lexerStartCodes = NE.cons i (lexerStartCodes st)
    }

-- | Pop a start code from the stack
popStartCode :: Lexer ()
popStartCode = modify' $ \st ->
  st
    { lexerStartCodes =
        case lexerStartCodes st of
          _ :| [] -> 0 :| []
          _ :| (x : xs) -> x :| xs
    }

-- | Get the current layout context
layout :: Lexer (Maybe Layout)
layout = gets (fmap fst . uncons . lexerLayout)

-- | Push a new layout context
pushLayout :: Layout -> Lexer ()
pushLayout i = modify' $ \st ->
  st {lexerLayout = i : lexerLayout st}

-- | Pop a layout context
popLayout :: Lexer ()
popLayout = modify' $ \st ->
  st
    { lexerLayout =
        case lexerLayout st of
          _ : xs -> xs
          [] -> []
    }

-- | Create initial lexer state
initState :: String -> LexerState
initState str =
  LS
    { lexerInput = Input 0 1 '\n' str,
      lexerStartCodes = 0 :| [],
      lexerLayout = []
    }

-- | Emit a token based on lexeme
emit :: (String -> Token) -> String -> Lexer Token
emit = (pure .)

-- | Return a constant token
token :: Token -> String -> Lexer Token
token = const . pure

-- | Run the lexer on input
runLexer :: Lexer a -> String -> Either String a
runLexer act s = fst <$> runStateT (_getLexer act) (initState s)
