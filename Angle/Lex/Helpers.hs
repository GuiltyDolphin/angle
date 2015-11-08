module Angle.Lex.Helpers
    ( choice
    , tryScan
    , cond
    , char
    , notChar
    , string
    , followed
    , lookAhead
    , within
    , sepWith
    , charFrom
    , surrounded
    , (<?>)
    , oneFrom
    , evalScan
    , Scanner
    ) where

import Angle.Scanner ( scanChar, unexpectedErr
                     , Scanner(..)
                     , ScanState(..)
                     , ScanError(..)
                     , ScanEnv(..)
                     ) 

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
import Control.Applicative

-- | Succeeds if the predicate function returns
--   true when passed the next character.
-- >>> evalScan "test" (cond (/='t'))
-- Left ...
-- ...
--
-- >>> evalScan "test" (cond (/='h'))
-- Right 't'
--
cond :: (Char -> Bool) -> Scanner Char
cond f = tryScan $ do
  ch <- scanChar
  if f ch then return ch
  else failScan . concat $ ["unexpected character: ", show ch]

-- Attempt to satisfy the provided scanner, but revert
-- the state upon failure.
tryScan :: Scanner a -> Scanner a
tryScan sc = do
  st <- get
  sc `catchError` (\e -> do
    put st
    throwError e)

-- | Match the specified character
-- >>> evalScan "test" (char 't')
-- Right 't'
--
-- >> evalScan "test" (char 'h')
-- Left (...)
char :: Char -> Scanner Char
char ch = cond (==ch) <?> show ch

-- >>> evalScan "test" (charFrom "some")
-- Right 'e'
--
-- >>> evalScan "test" (charFrom "test")
-- Right 't'
--
-- >>> evalScan "test" (charFrom "no")
-- Left (...)
charFrom :: String -> Scanner Char
charFrom str = cond (`elem` str)

-- Match an entire string
string :: String -> Scanner String
string str = tryScan (mapM char str) <?> str

-- Match scanner sc between start and end
within :: Scanner a -> Scanner b -> Scanner c -> Scanner c
within start end sc = do
  start
  res <- sc
  end
  return res

surrounded :: Scanner a -> Scanner b -> Scanner b
surrounded surr = tryScan . within surr surr

followed :: Scanner a -> Scanner b -> Scanner b
followed f sc = do
  res <- sc
  f
  return res
  -- tryScan (sc <* f)

-- Use first Scanner that matches
choice :: [Scanner a] -> Scanner a
choice = msum

-- Attempt each of xs as an input to `sc' and return first
-- successful result.
oneFrom :: (a -> Scanner a) -> [a] -> Scanner a
oneFrom scf xs = choice $ map scf xs

-- Succeeds if it does not parse the specified character
notChar :: Char -> Scanner Char
notChar ch = cond (/=ch)

parseNonGreedy :: Scanner a -> Scanner a
parseNonGreedy sc = do
  st <- get
  res <- sc
  put st
  return res

-- lookAhead :: Scanner a -> Scanner b -> Scanner b
-- lookAhead lh sc = tryScan (do
--   res <- sc
--   parseNonGreedy lh
--   return res)
lookAhead :: Scanner a -> Scanner a
lookAhead lh = do
  pos <- get
  res <- lh
  put pos
  return res




sepWith :: Scanner a -> Scanner b -> Scanner [b]
sepWith sep sc = tryScan (do
  fs <- sc
  s <- optional sep
  case s of
    Nothing -> return [fs]
    Just _ -> liftM (fs:) (sepWith sep sc))

-- Fail the scan with the location and provided message.
failScan :: String -> Scanner a
failScan msg = do
  pos <- liftM sourcePos get
  throwError ((strMsg msg) { errPos=pos })
  -- throwError . strMsg . concat $ [showPos pos, msg]

-- If the scan fails, specify what was expected
infix 0 <?>
(<?>) :: Scanner a -> String -> Scanner a
sc <?> msg = do
  oldPos <- liftM sourcePos get
  sc `catchError` (\e -> do
    newPos <- liftM sourcePos get
    if newPos == oldPos then throwError $ e { expectedMsg = msg, errPos=newPos }
    else throwError e)

-- Used for evaluating a single Scanner with a given string.
-- Assumes reasonable default state.
evalScan :: String -> Scanner a -> Either ScanError a
evalScan str sc = runReader (evalStateT (runErrorT (runScanner sc)) defaultState) env
  where defaultState = ScanState { sourcePos = (0,0,0) }
        env = ScanEnv { sourceText = str }
