{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module      : Angle.Scanner
Description : Defines the Parser type, and the base scanner function.
Copyright   : Copyright (C) 2015 Ben Moon
License     : GNU GPL, version 3
Maintainer  : GuiltyDolphin@gmail.com
Stability   : alpha

Defines the `Parser' monad along with the language scanner.

The main features are:

* 'Parser' @a@ - the type that represents a parser that will
turn the input stream into a type @a@.

* 'scanChar' - base function for yielding characters from the input
stream.

* 'evalParse' @stream parser@ - runs @parser@ on @stream@, producing a
result wrappied in 'Either'.
-}
module Angle.Scanner
  (
  -- * Types

  -- ** Parser
    Parser
  , evalParse

  -- ** Errors
  , ParseError(..)
  , (<?>)
  , unexpectedErr

  -- * Scanner
  , scanChar

  -- ** Position
  , SourcePos(..)
  , beginningOfFile
  , colNo
  , lineNo
  , sourcePos

  ) where


import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Monad.Except


-- | Represents a position in source.
newtype SourcePos = SourcePos
    { getSourcePos :: (Int, Int, Int) }
    deriving (Eq)


instance Show SourcePos where
    show sp =
        let ln = lineNo sp
            cn = colNo sp
        in concat ["line: ", show (ln+1), ", column: ", show (cn+1)]


-- | Start position in a file.
beginningOfFile :: SourcePos
beginningOfFile = SourcePos (0,0,0)


-- | Retrieve the line number from a source position.
lineNo :: SourcePos -> Int
lineNo (SourcePos (ln, _, _)) = ln


-- | Retrieve the column number from a source position.
colNo :: SourcePos -> Int
colNo (SourcePos (_, cn, _)) = cn


-- | Retrieve the source index from a source position.
-- sourceIndex :: SourcePos -> Int
-- sourceIndex (SourcePos (_, _, si)) = si


-- | Reset the column number whilst incrementing the
-- line number and source index.
incNL :: SourcePos -> SourcePos
incNL (SourcePos (ln,_,si)) = SourcePos (ln+1, 0, si+1)


-- | Increment the column and source index but keep the
-- line number constant.
incCol :: SourcePos -> SourcePos
incCol (SourcePos (ln,cn,si)) = SourcePos (ln,cn+1,si+1)


-- | Holds information about the current position in source.
data ScanState = ScanState
         { sourcePos :: SourcePos
         -- ^ The current position in source.
         , sourceRemaining :: String
         -- ^ Source that has not yet been traversed.
         , sourceScanned :: String
         -- ^ Source that has already been traversed.
         } deriving (Show, Eq)


-- | The environment variables that the parser can access.
data ParseEnv = ParseEnv
  { sourceText :: String
  } deriving (Show, Eq)


-- A @Parser a@ builds a type @a@ from an input stream.
newtype Parser a = Parser
    { runParser :: ExceptT ParseError
                    (StateT ScanState
                     (Reader ParseEnv)) a }
    deriving ( Functor, Applicative, Monad )


instance MonadError ParseError Parser where
    throwError = Parser . throwE
    catchError (Parser e) h
        = Parser (lift $ runExceptT e) >>= either h return


instance MonadState ScanState Parser where
    get = Parser $ lift get
    put x = Parser $ lift $ put x


instance MonadPlus Parser where
    mzero = do
      s <- get
      emptyErrh $ UnknownError (sourcePos s)
    mplus x y = x `catchError` const y


instance Alternative Parser where
    empty = mzero
    (<|>) = mplus


instance MonadReader ParseEnv Parser where
    ask = Parser $ lift ask
    local f (Parser (ExceptT e)) = Parser $ ExceptT $ local f e


emptyErrh :: ParseError -> Parser a
emptyErrh = throwError


-- | Type for tracking information about lexical errors.
data ParseError = ParseError
    { expectedMsg :: String -- ^Human readable statement of
                            -- an expected value
    , unexpectedMsg :: String
    , errMsg :: String    -- ^A general error message that
                          -- does not fit into one of the
                          -- above two categories
    , errPos :: SourcePos  -- ^The position in source
                           -- where the error occurred
    , parseErrText :: String -- Reference to the source text
    } | UnknownError SourcePos deriving (Eq)


instance Show ParseError where
  show (ParseError{errPos=ep, expectedMsg=em
                 , unexpectedMsg=um, errMsg=errm
                 , parseErrText=et})
    = cEp ++ cEt ++ cUm ++ cEm ++ errm
      where cEp = show ep ++ "\n"
            cEt = let lns = lines et in
                  if null lns
                  then "no source\n"
                  else replicate (colNo ep) ' ' ++ "v\n" ++ lns !! lineNo ep ++ "\n"
            cEm = if null em then ""
                  else concat ["expected ", em, "\n"]
            cUm = if null um then ""
                  else concat ["unexpected ", um, "\n"]
  show (UnknownError pos) = show pos ++ "\nUnknown Error!"


-- | Throws an error stating the unexpected input.
unexpectedErr :: String -> Parser a
unexpectedErr = toErr (\e msg -> e { unexpectedMsg=msg })


toErr :: (ParseError -> String -> ParseError) -> String -> Parser a
toErr err msg = do
  pos <- liftM sourcePos get
  txt <- liftM sourceText ask
  let e = basicErr { errPos=pos, parseErrText=txt }
  throwError (err e msg)


basicErr :: ParseError
basicErr = ParseError { errMsg=""
                     , unexpectedMsg=""
                     , expectedMsg=""
                     , errPos=beginningOfFile
                     , parseErrText = ""}


-- | Retrieves the next character from the
-- stream whilst updating the position.
--
-- Throws an error if it reaches the end of the stream.
scanChar :: Parser Char
scanChar = do
  st <- get
  let remSource = sourceRemaining st
      pos  = sourcePos st
  if remSource == ""
  then unexpectedErr "end of stream"
  else do
    let chr = head remSource
    put st{ sourcePos=if chr == '\n'
                      then incNL pos
                      else incCol pos
          , sourceRemaining=tail remSource
          , sourceScanned=chr : sourceScanned st
          }

    return chr


-- | Used for evaluating a single Parser with a given string.
--
-- Assumes reasonable default state.
evalParse :: String -> Parser a -> Either ParseError a
evalParse str sc = runReader (evalStateT (runExceptT (runParser sc)) defaultState) env
  where defaultState = ScanState { sourcePos = beginningOfFile
                                 , sourceRemaining = str
                                 , sourceScanned = ""
                                 }
        env = ParseEnv { sourceText = str
                      }


-- | If the parse fails, specify what was expected.
infix 0 <?>
(<?>) :: Parser a -> String -> Parser a
sc <?> msg = do
  oldPos <- liftM sourcePos get
  sc `catchError` (\e -> do
    newPos <- liftM sourcePos get
    if newPos == oldPos then throwError $ e {expectedMsg=msg}--expectedErr msg
    else throwError e)
