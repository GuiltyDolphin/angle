{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- Change imports
module Angle.Scanner
  ( runScanner
  , scanChar
  , unexpectedErr
  , Scanner
  , SourcePos
  , ScanEnv(..)
  , ScanState(..)
  , ScanError(..)
  ) where

import Control.Monad.State
import Data.List (genericIndex, genericLength)
import Control.Monad
import Control.Monad.Reader
import Control.Applicative
import Control.Monad.Error
-- Represents the (line number, column number, source index)
type SourcePos = (Integer, Integer, Integer)
-- Retrieve the line number from a source position
lineNo :: SourcePos -> Integer
lineNo (ln, _, _) = ln

-- Retrieve the column number from a source position
colNo :: SourcePos -> Integer
colNo (_, cn, _) = cn

-- Retrieve the source index from a source position
sourceIndex :: SourcePos -> Integer
sourceIndex (_, _, si) = si

-- |Reset the column number whilst incrementing the line number
-- and source index.
-- >>> incNL (0,0,0)
-- (1,0,1)
incNL :: SourcePos -> SourcePos
incNL (ln,_,si) = (ln+1, 0, si+1)

-- |Increment the column and source index but keep the
-- line number constant.
-- >>> incCol (0,0,0)
-- (0,1,1)
incCol :: SourcePos -> SourcePos
incCol (ln,cn,si) = (ln,cn+1,si+1)
-- Holds information about the current position in source.
data ScanState = ScanState { sourcePos :: SourcePos } deriving (Show, Eq)

-- The environment variables that the scanner can access.
data ScanEnv = ScanEnv { sourceText :: String } deriving (Show, Eq)
newtype Scanner a = Scanner { runScanner :: ErrorT ScanError (StateT ScanState (Reader ScanEnv)) a }
  deriving ( Functor, Applicative
           , Alternative, Monad, MonadPlus
           , MonadState ScanState
           , MonadReader ScanEnv, MonadError ScanError)

data ScanError = ScanError 
    { expectedMsg :: String -- Human readable statement of 
                            -- an expected value
    , unexpectedMsg :: String
    , errMsg :: String    -- A general error message that 
                          -- does not fit into one of the 
                          -- above two categories
    , errPos :: SourcePos  -- The position in source 
                           -- where the error occurred
    } deriving (Eq)

instance Error ScanError where
  noMsg = ScanError { errMsg = "", expectedMsg=""
                    , unexpectedMsg="", errPos=(0,0,0) }
  strMsg msg = noMsg { errMsg = msg }

instance Show ScanError where
  show (ScanError{errPos=ep, expectedMsg=em
                 , unexpectedMsg=um, errMsg=errm})
    = cEp ++ cUm ++ cEm ++ errm
      where cEp = showPos ep ++ "\n"
            cEm = if null em then "" 
                  else concat ["expected ", em, "\n"]
            cUm = if null um then "" 
                  else concat ["unexpected ", um, "\n"]
            showPos :: SourcePos -> String
            showPos (ln,cn,_) =
              concat ["line: ", show ln, ", column: ", show cn]
                     
unexpectedErr :: String -> Scanner a
unexpectedErr msg = throwError (noMsg { unexpectedMsg=msg })

scanChar :: Scanner Char
scanChar = do
  st <- get
  sourceString <- liftM sourceText ask
  let pos  = sourcePos st
      indx = sourceIndex pos
  if indx >= genericLength sourceString
  then unexpectedErr "end of stream"
  else do
    let chr = sourceString `genericIndex` indx
    put st{sourcePos=if chr == '\n'
                     then incNL pos  -- Error messages 
                                     -- more useful
                                     -- if tracking the 
                                     -- line number
                     else incCol pos}
    return chr
