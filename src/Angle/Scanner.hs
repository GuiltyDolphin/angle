{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module      : Angle.Scanner
Description : Defines the Scanner type.
Copyright   : Copyright (C) 2015 Ben Moon
License     : GNU GPL, version 3
Maintainer  : GuiltyDolphin@gmail.com
Stability   : alpha

Defines the language 'Scanner'.

__TODO__

= __TODO__

__TODO__
-}
module Angle.Scanner
  (
  -- * Types

  -- ** Scanner
    Scanner
  , evalScan
  , scanChar

  -- ** Errors
  , ScanError(..)
  , (<?>)
  , unexpectedErr

  -- ** Position
  , SourcePos(..)
  , beginningOfFile
  , colNo
  , lineNo
  , sourcePos
  ) where


import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.List (genericIndex, genericLength)


-- | Represents a position in source.
newtype SourcePos = SourcePos
    { getSourcePos :: (Int, Int, Int) }
    deriving (Eq)


instance Show SourcePos where
    show sp =
        let ln = lineNo sp
            cn = colNo sp
        in concat ["line: ", show ln, ", column: ", show cn]


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
sourceIndex :: SourcePos -> Int
sourceIndex (SourcePos (_, _, si)) = si


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
         , sourceRemaining :: String
         , sourceScanned :: String
         -- ^ The current position in source.
         } deriving (Show, Eq)


-- | The environment variables that the scanner can access.
data ScanEnv = ScanEnv
  { sourceText :: String
  } deriving (Show, Eq)


-- | Collects results from the scanner and
-- returns a value of type @a@.
--
-- The scanner is the component of Angle that reads in characters
-- from the source text one at a time and passes them to the lexer to be
-- converted to tokens.
-- See <http://forums.devshed.com/programming-languages-139/interpreter-compiler-312483.html\#post1342279 this page>
-- for an overview of the role of a scanner.
newtype Scanner a = Scanner
    { runScanner :: ExceptT ScanError
                    (StateT ScanState
                     (Reader ScanEnv)) a }
    deriving ( Functor, Applicative, Monad )


instance MonadError ScanError Scanner where
    throwError = Scanner . throwE
    catchError (Scanner e) h
        = Scanner (lift $ runExceptT e) >>= either h return


instance MonadState ScanState Scanner where
    get = Scanner $ lift get
    put x = Scanner $ lift $ put x


instance MonadPlus Scanner where
    mzero = do
      s <- get
      emptyErrh $ UnknownError (sourcePos s)
    mplus x y = x `catchError` const y


instance Alternative Scanner where
    empty = mzero
    (<|>) = mplus


instance MonadReader ScanEnv Scanner where
    ask = Scanner $ lift ask
    local f (Scanner (ExceptT e)) = Scanner $ ExceptT $ local f e


emptyErrh :: ScanError -> Scanner a
emptyErrh = throwError


-- | Type for tracking information about lexical errors.
data ScanError = ScanError
    { expectedMsg :: String -- ^Human readable statement of
                            -- an expected value
    , unexpectedMsg :: String
    , errMsg :: String    -- ^A general error message that
                          -- does not fit into one of the
                          -- above two categories
    , errPos :: SourcePos  -- ^The position in source
                           -- where the error occurred
    , scanErrText :: String -- Reference to the source text
    } | UnknownError SourcePos deriving (Eq)


instance Show ScanError where
  show (ScanError{errPos=ep, expectedMsg=em
                 , unexpectedMsg=um, errMsg=errm
                 , scanErrText=et})
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
unexpectedErr :: String -> Scanner a
unexpectedErr = toErr (\e msg -> e { unexpectedMsg=msg })


toErr :: (ScanError -> String -> ScanError) -> String -> Scanner a
toErr err msg = do
  pos <- liftM sourcePos get
  txt <- liftM sourceText ask
  let e = basicErr { errPos=pos, scanErrText=txt }
  throwError (err e msg)


basicErr :: ScanError
basicErr = ScanError { errMsg=""
                     , unexpectedMsg=""
                     , expectedMsg=""
                     , errPos=beginningOfFile
                     , scanErrText = ""}


-- | Retrieves the next character from the
-- stream whilst updating the position.
--
-- Throws an error if it reaches the end of the stream.
scanChar :: Scanner Char
scanChar = do
  st <- get
  let remSource = sourceRemaining st
      pos  = sourcePos st
      indx = sourceIndex pos
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


-- | Used for evaluating a single Scanner with a given string.
--
-- Assumes reasonable default state.
evalScan :: String -> Scanner a -> Either ScanError a
evalScan str sc = runReader (evalStateT (runExceptT (runScanner sc)) defaultState) env
  where defaultState = ScanState { sourcePos = beginningOfFile
                                 , sourceRemaining = str
                                 , sourceScanned = ""
                                 }
        env = ScanEnv { sourceText = str
                      }


-- | If the scan fails, specify what was expected.
infix 0 <?>
(<?>) :: Scanner a -> String -> Scanner a
sc <?> msg = do
  oldPos <- liftM sourcePos get
  sc `catchError` (\e -> do
    newPos <- liftM sourcePos get
    if newPos == oldPos then throwError $ e {expectedMsg=msg}--expectedErr msg
    else throwError e)
