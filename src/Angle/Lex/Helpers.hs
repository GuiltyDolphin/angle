{-|
Module      : Angle.Lex.Helpers
Description : Defines functions for working with the scanner.
Copyright   : Copyright (C) 2015 Ben Moon
License     : GNU GPL, version 3
Maintainer  : GuiltyDolphin@gmail.com
Stability   : alpha

Provided functions are split into two categories:

[@basic@] the functions that work on standard types.

[@advanced@] higher-order scanners.
-}
module Angle.Lex.Helpers
    (
    -- ** Basic
      anyChar
    , char
    , charFrom
    , cond
    , notChar
    , string

    -- ** Advanced
    , choice
    , followed
    , manyTill
    , noneFrom
    , notScan
    , sepWith
    , surrounded
    , tryScan
    , within
    , lookAhead

    -- ** Other
    , evalScan
    , Scanner
    , SourcePos
    , sourcePos
    , unexpectedErr
    , (<?>)
    ) where


import Control.Applicative
import Control.Monad.Except
import Control.Monad.State

import Angle.Scanner


-- | Succeeds if the predicate function returns
-- true when passed the next character.
cond :: (Char -> Bool) -> Scanner Char
cond f = tryScan $ do
  ch <- scanChar
  if f ch then return ch
  else unexpectedErr ("character: " ++ show ch)


-- | Attempt to satisfy the provided scanner, but revert
-- the state upon failure.
tryScan :: Scanner a -> Scanner a
tryScan sc = do
  st <- get
  sc `catchError` (\e -> do
    put st
    throwError e)


-- | Match the specified character.
char :: Char -> Scanner Char
char ch = cond (==ch) <?> show ch


-- | Matches if character is an element of the provided string.
charFrom :: String -> Scanner Char
charFrom str = cond (`elem` str)


-- | Match `str' in its entirety.
string :: String -> Scanner String
string str = tryScan (mapM char str) <?> str


-- | @within start end sc@ matches @sc@ between @start@ and @end@.
within :: Scanner a -> Scanner b -> Scanner c -> Scanner c
within start end sc = start *> sc <* end


-- | @surrounded x@ is the same as @within x x@.
surrounded :: Scanner a -> Scanner b -> Scanner b
surrounded surr = within surr surr


-- | Parses second scanner before first scanner, returning the result
-- of the second scanner.
followed :: Scanner a -> Scanner b -> Scanner b
followed f sc = sc <* f


-- | Use first Scanner that succeeds.
choice :: [Scanner a] -> Scanner a
choice = msum




-- | Succeeds if it does not parse the specified character.
notChar :: Char -> Scanner Char
notChar ch = cond (/=ch)


-- | Matches any character, only fails when there is no more input.
anyChar :: Scanner Char
anyChar = scanChar <?> "any character"


-- | Succeeds if the passed scanner succeeds, but does not consume
-- input upon success.
lookAhead :: Scanner a -> Scanner a
lookAhead sc = do
  pos <- get
  res <- sc
  put pos
  return res


-- | Succeeds only if `sc' does not succeed.
notScan :: (Show a) => Scanner a -> Scanner ()
notScan sc = tryScan (do
  res <- optional (tryScan (lookAhead sc))
  case res of Nothing -> return ()
              Just x -> unexpectedErr (show x))


-- | @noneFrom sc scs@ builds a list of scanners by
-- applying @sc@ to each of @scs@, the resultant scanner
-- then succeeds only if all of the resultant scanners
-- fail.
noneFrom :: (Show a) => (a -> Scanner a) -> [a] -> Scanner ()
noneFrom scf = notScan . oneFrom
  where oneFrom xs = choice $ map scf xs


-- | List of `sc' separated with `sep'.
sepWith :: Scanner a -> Scanner b -> Scanner [b]
sepWith sep sc = tryScan (do
  fsm <- optional sc
  case fsm of
    Nothing -> return []
    Just fs -> do
        s <- optional sep
        case s of
          Nothing -> return [fs]
          Just _ -> liftM (fs:) (sepWith sep sc))


-- | Collect sc until `ti' succeeds.
manyTill :: (Show b) => Scanner b -> Scanner a -> Scanner [a]
manyTill ti sc = many (notScan ti *> sc)












