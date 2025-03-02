{-# LANGUAGE OverloadedStrings #-}

module Zayin.Lexer (Token(..), tokenize) where

import qualified Data.Text as T
import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)
import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Except

-- Token representation
data Token
  = TWord T.Text        -- Identifier or variable name
  | TString T.Text      -- String literal
  | TInteger Integer    -- Integer literal
  | TFloat Float        -- Float literal
  | TBool Bool          -- Boolean literal
  | TNil                -- None literal
  | TColon              -- :
  | TLeftBracket        -- [
  | TRightBracket       -- ]
  | TLeftBrace          -- {
  | TRightBrace         -- }
  | TLeftParen          -- (
  | TRightParen         -- )
  | TEither             -- either keyword
  | TFunc               -- func keyword
  | TContext            -- context keyword
  deriving (Show, Eq)

-- Lexer state
data LexState = LexState
  { input :: T.Text        -- Remaining input
  , position :: (Int, Int) -- Line and column
  , tokens :: [Token]      -- Accumulated tokens
  } deriving (Show)

-- Lexer monad
type Lexer a = StateT LexState (Either String) a

-- Initialize a lexer state
initialState :: T.Text -> LexState
initialState input = LexState input (1, 1) []

-- Advance position
advancePos :: Char -> (Int, Int) -> (Int, Int)
advancePos '\n' (line, _) = (line + 1, 1)
advancePos _ (line, col) = (line, col + 1)

-- Consume a character and advance position
advance :: Lexer ()
advance = do
  s <- get
  case T.uncons (input s) of
    Nothing -> return ()
    Just (c, rest) ->
      put s { input = rest, position = advancePos c (position s) }

-- Check if input is exhausted
eof :: Lexer Bool
eof = gets (T.null . input)

-- Peek at the next character
peek :: Lexer (Maybe Char)
peek = do
  s <- get
  return $ if T.null (input s) then Nothing else Just (T.head (input s))

-- Skip whitespace
skipWhitespace :: Lexer ()
skipWhitespace = do
  maybeChar <- peek
  case maybeChar of
    Just c | isSpace c -> do
      advance
      skipWhitespace
    _ -> return ()

-- Skip comments (lines starting with ;)
skipComment :: Lexer ()
skipComment = do
  maybeChar <- peek
  case maybeChar of
    Just ';' -> do
      advance
      skipToEndOfLine
    _ -> return ()
  where
    skipToEndOfLine = do
      maybeChar <- peek
      case maybeChar of
        Just '\n' -> advance
        Just _ -> do
          advance
          skipToEndOfLine
        Nothing -> return ()

-- Skip whitespace and comments
skipWhitespaceAndComments :: Lexer ()
skipWhitespaceAndComments = do
  skipWhitespace
  maybeChar <- peek
  case maybeChar of
    Just ';' -> do
      skipComment
      skipWhitespaceAndComments
    _ -> return ()

-- Add a token to the state
addToken :: Token -> Lexer ()
addToken token = modify $ \s -> s { tokens = token : tokens s }

-- Tokenize a word (keyword or identifier)
tokenizeWord :: Lexer ()
tokenizeWord = do
  s <- get
  let (word, rest) = T.span (\c -> isAlphaNum c || c == '-' || c == '?' || c == '!') (input s)
  let token = case word of
                "either" -> TEither
                "func" -> TFunc
                "context" -> TContext
                "true" -> TBool True
                "false" -> TBool False
                "none" -> TNil
                _ -> TWord word

  -- Update state
  let newPos = foldl (flip advancePos) (position s) (T.unpack word)
  put s { input = rest, position = newPos }
  addToken token

-- Tokenize a string literal
tokenizeString :: Lexer ()
tokenizeString = do
  quoteChar <- peek
  case quoteChar of
    Just '"' -> tokenizeDoubleQuoteString
    Just '{' -> tokenizeBraceString
    _ -> throwError "Expected string delimiter"

-- Tokenize a double-quoted string
tokenizeDoubleQuoteString :: Lexer ()
tokenizeDoubleQuoteString = do
  advance  -- Skip opening quote
  s <- get
  let (content, rest) = T.break (== '"') (input s)
  if T.null rest
    then throwError $ "Unterminated string at " ++ show (position s)
    else do
      let newPos = foldl (flip advancePos) (position s) (T.unpack content)
      put s { input = T.tail rest, position = advancePos '"' newPos }
      addToken (TString content)

-- Tokenize a brace-enclosed string
tokenizeBraceString :: Lexer ()
tokenizeBraceString = do
  advance  -- Skip opening brace
  collectBraceContent 1 ""
  where
    collectBraceContent :: Int -> T.Text -> Lexer ()
    collectBraceContent 0 acc = addToken (TString acc)
    collectBraceContent level acc = do
      maybeChar <- peek
      case maybeChar of
        Nothing -> throwError "Unterminated brace string"
        Just '{' -> do
          advance
          collectBraceContent (level + 1) (T.snoc acc '{')
        Just '}' -> do
          advance
          if level == 1
            then addToken (TString acc)
            else collectBraceContent (level - 1) (T.snoc acc '}')
        Just c -> do
          advance
          collectBraceContent level (T.snoc acc c)

-- Tokenize a number (integer or float)
tokenizeNumber :: Lexer ()
tokenizeNumber = do
  s <- get
  let (numText, rest) = T.span (\c -> isDigit c || c == '.' || c == '-') (input s)

  if T.isInfixOf "." numText
    then do
      -- It's a float
      let floatVal = read (T.unpack numText) :: Float
      let newPos = foldl (flip advancePos) (position s) (T.unpack numText)
      put s { input = rest, position = newPos }
      addToken (TFloat floatVal)
    else do
      -- It's an integer
      let intVal = read (T.unpack numText) :: Integer
      let newPos = foldl (flip advancePos) (position s) (T.unpack numText)
      put s { input = rest, position = newPos }
      addToken (TInteger intVal)

-- Tokenize a single token
-- Tokenize a single token
tokenizeOne :: Lexer Bool
tokenizeOne = do
  skipWhitespaceAndComments
  done <- eof
  if done
    then return True
    else do
      maybeChar <- peek
      case maybeChar of
        Just c -> do
          case c of
            ':' -> advance >> addToken TColon
            '[' -> advance >> addToken TLeftBracket
            ']' -> advance >> addToken TRightBracket
            '{' -> tokenizeString
            '}' -> advance >> addToken TRightBrace
            '(' -> advance >> addToken TLeftParen
            ')' -> advance >> addToken TRightParen
            '"' -> tokenizeString
            '-' -> do
              -- Check if this is the start of a negative number
              advance
              nextChar <- peek
              case nextChar of
                Just d | isDigit d -> do
                  -- Put back the minus sign and tokenize as number
                  modify $ \s -> s { input = T.cons '-' (input s) }
                  tokenizeNumber
                _ -> do
                  -- Not a number, treat as part of an identifier
                  modify $ \s -> s { input = T.cons '-' (input s) }
                  tokenizeWord
            _ | isDigit c -> tokenizeNumber
              | isAlpha c -> tokenizeWord
              | otherwise -> throwError $ "Unexpected character: " ++ [c]
          return False
        Nothing -> return True

-- Main tokenize function
tokenize :: T.Text -> Either String [Token]
tokenize input = do
  (_, finalState) <- runStateT tokenizeAll (initialState input)
  return $ reverse (tokens finalState)
  where
    tokenizeAll :: Lexer ()
    tokenizeAll = do
      done <- tokenizeOne
      if done then return () else tokenizeAll
