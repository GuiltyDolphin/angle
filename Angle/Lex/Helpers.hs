module Angle.Lex.Helpers
    ( choice
    , tryScan
    , notScan
    , cond
    , char
    , notChar
    , noneFrom
    , string
    , followed
    , lookAhead
    , within
    , sepWith
    , charFrom
    , surrounded
    , (<?>)
    , oneFrom
    , anyChar
    , manyTill
    , someTill
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
  else unexpectedErr ("character: " ++ show ch)  -- failScan . concat $ ["unexpected character: ", show ch]

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

-- |Match `str' in its entirety
-- >>> evalScan "test" (string "test")
-- Right "test"
--
-- >>> evalScan "test" (string "testing")
-- Left ...
-- ...
string :: String -> Scanner String
string str = tryScan (mapM char str) <?> str

-- |Match scanner `sc' between `start' and `end'
-- >>> evalScan "(1)" (within (char '(') (char ')') (char '1'))
-- Right '1'
within :: Scanner a -> Scanner b -> Scanner c -> Scanner c
within start end sc = do
  start
  res <- sc
  end
  return res

-- |Like `within', but where `start' and `end' are the same
-- >>> evalScan "'one'" (surrounded (char '\'') (string "one"))
-- Right "one"
--
surrounded :: Scanner a -> Scanner b -> Scanner b
surrounded surr = tryScan . within surr surr

-- |`f' succeeds after `sc'
-- >>> evalScan "test" (followed (char 'e') (char 't'))
-- Right 't'
--
-- >>> evalScan "test" (followed (char 's') (char 't'))
-- Left ...
-- ...
followed :: Scanner a -> Scanner b -> Scanner b
followed f sc = do
  res <- sc
  f
  return res
  -- tryScan (sc <* f)

-- Use first Scanner that succeeds
-- evalScan "test" (choice [char 'e', char 't'])
-- Right 't'
--
-- evalScan "test" (choice [char 'e', char 's'])
-- Left ...
-- ...
choice :: [Scanner a] -> Scanner a
choice = msum

-- |Attempt each of xs as an input to `sc' and return first
-- successful result.
-- >>> evalScan "test" (oneFrom char "et")
-- Right 't'
oneFrom :: (a -> Scanner a) -> [a] -> Scanner a
oneFrom scf xs = choice $ map scf xs

-- |Succeeds if it does not parse the specified character
-- >>> evalScan "test" (notChar 'e')
-- Right 't'
--
-- >>> evalScan "test" (notChar 't')
-- Left ...
-- ...
notChar :: Char -> Scanner Char
notChar ch = cond (/=ch)
             
-- |Matches any character, used for determining eof
anyChar = scanChar <?> "any character"

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
lookAhead sc = do
  pos <- get
  res <- sc
  put pos
  return res

-- |Succeeds only if `sc' does not match
-- >>> evalScan "hello" (notScan (char 'h') *> char 'h')
-- Left ...
-- ...
--
-- >>> evalScan "hello" (notScan (char 't') *> char 'h')
-- Right 'h'
--
-- TODO: Might want to have (Show a) =>
--  to allow a reasonable error message where
--  the character is printed
-- TODO: Try to find a way of setting the
--  unexpected error message to the originally
--  expected error message
notScan :: Scanner a -> Scanner ()
notScan sc = tryScan (do
  res <- optional (tryScan (lookAhead sc))
  case res of Nothing -> return ()
              Just _ -> unexpectedErr "notscan")


-- |Fail if any scanners built from `scf' succeed
-- >>> evalScan "test" (noneFrom char "abc")
-- Right ()
--
-- >>> evalScan "test" (noneFrom char "tea")
-- Left ...
-- ...
noneFrom :: (a -> Scanner a) -> [a] -> Scanner ()
noneFrom scf = notScan . oneFrom scf
               

-- [[[
chain :: [Scanner a] -> Scanner [a]
chain = sequence
        
chainFlat :: [Scanner [a]] -> Scanner [a]
chainFlat = liftM concat . chain
-- ]]]

-- |List of `sc' separated with `sep'
-- >>> evalScan "1,2,3" (sepWith (char ',') (charFrom ['1'..'9']))
-- Right "123"
sepWith :: Scanner a -> Scanner b -> Scanner [b]
sepWith sep sc = tryScan (do
  fs <- sc
  s <- optional sep
  case s of
    Nothing -> return [fs]
    Just _ -> liftM (fs:) (sepWith sep sc))

-- |Collect sc until `ti' succeeds
-- >>> evalScan "abc.def" (manyTill (char '.') anyChar)
-- Right "abc"
--
-- >>> evalScan ".abc" (manyTill (char '.') anyChar)
-- Right ""
manyTill :: Scanner b -> Scanner a -> Scanner [a]
manyTill ti sc = many (notScan ti *> sc)
                 
-- |Like `manyTill', but `sc' must succeed before `ti'
-- >>> evalScan "123.456" (someTill (char '.') anyChar)
-- Right "123"
--
-- >>> evalScan ".123" (someTill (char '.') anyChar)
-- Left ...
-- ...
someTill :: Scanner b -> Scanner a -> Scanner [a]
someTill ti sc = (:) <$> (notScan ti *> sc) <*> manyTill ti sc

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

