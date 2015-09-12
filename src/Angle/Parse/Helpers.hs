{-|
Module      : Angle.Parse.Helpers
Description : Defines functions for working with the parser.
Copyright   : Copyright (C) 2015 Ben Moon
License     : GNU GPL, version 3
Maintainer  : GuiltyDolphin@gmail.com
Stability   : alpha

Provided functions are split into two categories:

[@basic@] the functions that work on standard types.

[@advanced@] higher-order parsers.
-}
module Angle.Parse.Helpers
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
    , notParse
    , sepWith
    , surrounded
    , tryParse
    , within
    , lookAhead

    -- ** Other
    , evalParse
    , Parser
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
cond :: (Char -> Bool) -> Parser Char
cond f = tryParse $ do
  ch <- scanChar
  if f ch then return ch
  else unexpectedErr ("character: " ++ show ch)


-- | Attempt to satisfy the provided parser, but revert
-- the state upon failure.
tryParse :: Parser a -> Parser a
tryParse sc = do
  st <- get
  sc `catchError` (\e -> do
    put st
    throwError e)


-- | Match the specified character.
char :: Char -> Parser Char
char ch = cond (==ch) <?> show ch


-- | Matches if character is an element of the provided string.
charFrom :: String -> Parser Char
charFrom str = cond (`elem` str)


-- | Match `str' in its entirety.
string :: String -> Parser String
string str = tryParse (mapM char str) <?> str


-- | @within start end sc@ matches @sc@ between @start@ and @end@.
within :: Parser a -> Parser b -> Parser c -> Parser c
within start end sc = start *> sc <* end


-- | @surrounded x@ is the same as @within x x@.
surrounded :: Parser a -> Parser b -> Parser b
surrounded surr = within surr surr


-- | Parses second parser before first parser, returning the result
-- of the second parser.
followed :: Parser a -> Parser b -> Parser b
followed f sc = sc <* f


-- | Use first Parser that succeeds.
choice :: [Parser a] -> Parser a
choice = msum




-- | Succeeds if it does not parse the specified character.
notChar :: Char -> Parser Char
notChar ch = cond (/=ch)


-- | Matches any character, only fails when there is no more input.
anyChar :: Parser Char
anyChar = scanChar <?> "any character"


-- | Succeeds if the parser succeeds, but does not consume
-- input upon success.
lookAhead :: Parser a -> Parser a
lookAhead sc = do
  pos <- get
  res <- sc
  put pos
  return res


-- | Succeeds only if `sc' does not succeed.
notParse :: (Show a) => Parser a -> Parser ()
notParse sc = tryParse (do
  res <- optional (tryParse (lookAhead sc))
  case res of Nothing -> return ()
              Just x -> unexpectedErr (show x))


-- | @noneFrom sc scs@ builds a list of parsers by
-- applying @sc@ to each of @scs@, the resultant parser
-- then succeeds only if all of the resultant parsers
-- fail.
noneFrom :: (Show a) => (a -> Parser a) -> [a] -> Parser ()
noneFrom scf = notParse . oneFrom
  where oneFrom xs = choice $ map scf xs


-- | List of `sc' separated with `sep'.
sepWith :: Parser a -> Parser b -> Parser [b]
sepWith sep sc = tryParse (do
  fsm <- optional sc
  case fsm of
    Nothing -> return []
    Just fs -> do
        s <- optional sep
        case s of
          Nothing -> return [fs]
          Just _ -> liftM (fs:) (sepWith sep sc))


-- | Collect sc until `ti' succeeds.
manyTill :: (Show b) => Parser b -> Parser a -> Parser [a]
manyTill ti sc = many (notParse ti *> sc)












