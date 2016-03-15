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
      notChar

    -- ** Advanced
    , followed
    , noneFrom
    , surrounded

    -- ** Other
    , Parser
    , evalParse
    ) where


import Control.Applicative ((<*))
import Control.Monad.State

import Text.Parsec

type Parser st a = Parsec String st a

evalParse s p = runParser p "" "" s

-- | @surrounded x@ is the same as @within x x@.
surrounded :: Parser st a -> Parser st b -> Parser st b
surrounded surr = between surr surr

-- | Parses second parser before first parser, returning the result
-- of the second parser.
followed :: Parser st a -> Parser st b -> Parser st b
followed f sc = sc <* f

-- | Succeeds if it does not parse the specified character.
notChar :: Char -> Parser st Char
notChar ch = satisfy (/=ch)

-- | @noneFrom sc scs@ builds a list of parsers by
-- applying @sc@ to each of @scs@, the resultant parser
-- then succeeds only if all of the resultant parsers
-- fail.
noneFrom :: (Show a) => (a -> Parser st a) -> [a] -> Parser st ()
noneFrom f ps = notFollowedBy (choice $ map f ps)
