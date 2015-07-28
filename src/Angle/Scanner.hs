{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- Change imports
module Angle.Scanner
  ( runScanner
  , scanChar
  , beginningOfFile
  , unexpectedErr
  , expectedErr
  , Scanner
  , SourcePos(..)
  , lineNo
  , colNo
  , ScanEnv(..)
  , ScanState(..)
  , ScanError(..)
  ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Data.List (genericIndex, genericLength)


-- | Represents a position in source.
newtype SourcePos = SourcePos 
    { getSourcePos :: (Int, Int, Int) }
    deriving (Eq)


instance Show SourcePos where
    show (SourcePos (ln,cn,_)) =
        concat ["line: ", show ln, ", column: ", show cn]


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
data ScanState = ScanState { sourcePos :: SourcePos } deriving (Show, Eq)


-- | The environment variables that the scanner can access.
data ScanEnv = ScanEnv { sourceText :: String } deriving (Show, Eq)


-- | Collects results from the scanner and
-- returns a value of type @a@.
newtype Scanner a = Scanner 
    { runScanner :: ErrorT ScanError (StateT ScanState (Reader ScanEnv)) a }
  deriving ( Functor, Applicative
           , Alternative, Monad, MonadPlus
           , MonadState ScanState
           , MonadReader ScanEnv, MonadError ScanError)


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
    } deriving (Eq)


instance Error ScanError where
  noMsg = ScanError { errMsg = "", expectedMsg=""
                    , unexpectedMsg="", errPos=beginningOfFile
                    , scanErrText=""
                    }
  strMsg msg = noMsg { errMsg = msg }


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
                     

unexpectedErr :: String -> Scanner a
unexpectedErr msg = do
  pos <- liftM sourcePos get
  txt <- liftM sourceText ask
  throwError (noMsg { unexpectedMsg=msg, errPos=pos, scanErrText=txt })
             

expectedErr :: String -> Scanner a
expectedErr msg = do
  pos <- liftM sourcePos get
  txt <- liftM sourceText ask
  throwError (noMsg { expectedMsg=msg, errPos=pos, scanErrText=txt})


-- | Retrieves the next character from the
-- stream whilst updating the position.
--
-- Throws an error if it reaches the end of the stream.
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
                     then incNL pos
                     else incCol pos}
    return chr
