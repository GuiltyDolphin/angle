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
    -- , sourcePos
    -- , unexpectedErr
    , (<?>)
    ) where


import Control.Applicative ((<*), (<*>), (*>))
import Control.Monad.Except
import Control.Monad.State

-- import Angle.Scanner

import Text.Parsec

type Parser st a = Parsec String st a

evalParse s p = runParser p "" "" s

cond = satisfy
-- | Succeeds if the predicate function returns
-- true when passed the next character.
-- cond :: (Char -> Bool) -> Parser st Char
-- cond f = tryParse $ do
--   ch <- scanChar
--   if f ch then return ch
--   else unexpectedErr ("character: " ++ show ch)


-- | Attempt to satisfy the provided parser, but revert
-- the state upon failure.
tryParse :: Parser st a -> Parser st a
tryParse = try
-- tryParse sc = do
--   st <- get
--   sc `catchError` (\e -> do
--     put st
--     throwError e)


-- | Match the specified character.
-- char :: Char -> Parser st Char
-- char ch = cond (==ch) <?> show ch


-- | Matches if character is an element of the provided string.
charFrom :: String -> Parser st Char
charFrom str = cond (`elem` str)


-- | Match `str' in its entirety.
-- string :: String -> Parser st String
-- string str = tryParse (mapM char str) <?> str


-- | @within start end sc@ matches @sc@ between @start@ and @end@.
within :: Parser st a -> Parser st b -> Parser st c -> Parser st c
within start end sc = start *> sc <* end


-- | @surrounded x@ is the same as @within x x@.
surrounded :: Parser st a -> Parser st b -> Parser st b
surrounded surr = within surr surr


-- | Parses second parser before first parser, returning the result
-- of the second parser.
followed :: Parser st a -> Parser st b -> Parser st b
followed f sc = sc <* f


-- | Use first Parser st that succeeds.
-- choice :: [Parser st a] -> Parser st a
-- choice = msum




-- | Succeeds if it does not parse the specified character.
notChar :: Char -> Parser st Char
notChar ch = cond (/=ch)


-- | Matches any character, only fails when there is no more input.
-- anyChar :: Parser st Char
-- anyChar = scanChar <?> "any character"


-- | Succeeds if the parser succeeds, but does not consume
-- input upon success.
-- lookAhead :: Parser st a -> Parser st a
-- lookAhead sc = do
--   pos <- get
--   res <- sc
--   put pos
--   return res


-- | Succeeds only if `sc' does not succeed.
notParse :: (Show a) => Parser st a -> Parser st ()
notParse = notFollowedBy
-- notParse sc = tryParse (do
--   res <- optional (tryParse (lookAhead sc))
--   case res of Nothing -> return ()
--               Just x -> unexpectedErr (show x))


-- | @noneFrom sc scs@ builds a list of parsers by
-- applying @sc@ to each of @scs@, the resultant parser
-- then succeeds only if all of the resultant parsers
-- fail.
noneFrom :: (Show a) => (a -> Parser st a) -> [a] -> Parser st ()
-- noneFrom scf = notParse . oneFrom
--   where oneFrom xs = choice $ map scf xs
noneFrom f ps = notFollowedBy (choice $ map f ps)


-- | List of `sc' separated with `sep'.
sepWith :: Parser st a -> Parser st b -> Parser st [b]
sepWith = flip sepEndBy
-- sepWith sep sc = tryParse (do
--   fsm <- optional sc
--   case fsm of
--     Nothing -> return []
--     Just fs -> do
--         s <- optional sep
--         case s of
--           Nothing -> return [fs]
--           Just _ -> liftM (fs:) (sepWith sep sc))


-- | Collect sc until `ti' succeeds.
-- manyTill :: (Show b) => Parser st b -> Parser st a -> Parser st [a]
-- manyTill ti sc = many (notParse ti *> sc)












