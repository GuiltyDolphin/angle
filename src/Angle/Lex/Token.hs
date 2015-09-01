{-|
Module      : Angle.Lex.Token
Description : Definitons for language tokens.
Copyright   : Copyright (C) 2015 Ben Moon
License     : GNU GPL, version 3
Maintainer  : GuiltyDolphin@gmail.com
Stability   : alpha

TODO
-}
module Angle.Lex.Token
    (
    -- ** Whitespace
      tokNSpaced
    , tokSpace
    , tokWhitespace
    , whitespace

    -- ** Statements
    , tokAssign
    , tokStmtBetween
    , tokMultiStmtEnd
    , tokMultiStmtStart

    -- ** Literals
    , tokFloat
    , tokInt
    , tokList

    -- *** Strings
    , tokString
    , withCharEscape

    -- ** Misc
    , keywords
    , tokEOF
    , ident
    , parens
    , tokOpChar
    , tokEltSep
    ) where


import Control.Applicative
import Control.Monad
import Data.Char
import Numeric

import Angle.Lex.Helpers


tokMultiStmtStart :: Scanner Char
tokMultiStmtStart = surrounded whitespace (char '{')
                                    <?> "start of multi-statement"


tokMultiStmtEnd :: Scanner Char
tokMultiStmtEnd   = surrounded whitespace (char '}')
                                    <?> "end of multi-statement"


tokEltSep :: Scanner Char
tokEltSep         = surrounded whitespace (char ',')
                                    <?> "element separator"


tokDenaryDigit :: Scanner Char
tokDenaryDigit    = cond isDigit    <?> "denary digit"


-- | A single space character.
tokSpace :: Scanner Char
tokSpace          = char ' '        <?> "space"


-- Might be an issue with this
tokWhitespace :: Scanner Char
tokWhitespace     = cond isSpace    <?> "whitespace"


-- | Assignment operator character.
tokAssign :: Scanner Char
tokAssign = surrounded spaces (char '=')
                <?> "assignment operator"
  where
    spaces = many tokSpace


whitespace :: Scanner String
whitespace = many tokWhitespace


-- | Matches when there are no more characters in the stream.
tokEOF :: Scanner ()
tokEOF            = notScan anyChar


tokStmtBetween :: Scanner String
tokStmtBetween    = whitespace      <?> "ignored characters"


-- | Matches a string representing an integer.
tokInt :: (Integral a, Read a) => Scanner a
tokInt = do
  negve <- optional (char '-')
  res <- read <$> tokDigits <?> "integer"
  case negve of
    Nothing -> return res
    Just _  -> return (-res)


tokDigits :: Scanner String
tokDigits = some tokDenaryDigit


-- TODO: Might be able to use a standard read?
tokFloat :: Scanner Double
tokFloat = do
  negve <- optional (char '-')
  f <- tokDigits
  rst <- (:) <$> char '.' <*> tokDigits
  let res = read $ f ++ rst
  case negve of
    Nothing -> return res
    Just _  ->  return (-res)


tokList :: Scanner a -> Scanner a
tokList = within tokListStart tokListEnd
  where
    tokListStart      = char '['
    tokListEnd        = char ']'




-- | Function/variable identifier (but not a keyword).
ident :: Scanner String
ident = noneFrom (\x -> string x <* specEnd) keywords *> ((:) <$> tokIdentStartChar <*> many tokIdentBodyChar)
    where specEnd = notScan tokIdentBodyChar
          tokIdentStartChar = cond isAlpha
          tokIdentBodyChar  = cond isAlphaNum


tokOpChar :: Scanner Char
tokOpChar = charFrom "*/+->=<|&^"


keywords :: [String]
keywords = [ "break"
           , "continue"
           , "defclass"
           , "defun"
           , "do"
           , "else"
           , "false"
           , "for"
           , "if"
           , "in"
           , "null"
           , "return"
           , "then"
           , "true"
           , "unless"
           , "when"
           , "while"]


-- | Matches within parentheses.
parens :: Scanner a -> Scanner a
parens sc = within tokParenL tokParenR sc
            <?> "parentheses"
  where
    tokParenL = char '('
    tokParenR = char ')'




-- tokString :: Scanner String
-- tokString = within tokStringStart tokStringEnd
--             (many tokStringBodyChar) <?> "string"
--   where
--     tokStringStart    = char '"'
--     tokStringEnd      = char '"'
--     tokStringBodyChar = notChar '"'



stringNorm :: Scanner String
stringNorm = do
  char '"'
  r <- manyTill (char '"') (withCharEscape False)
  char '"'
  return (concat r)


tokString :: Scanner String
tokString = tryScan stringBS <|> stringNorm

stringBS :: Scanner String
stringBS = do
  string "e\""
  r <- manyTill (char '"') (withCharEscape True)
  char '"'
  return (concat r)


-- | Space or newline followed by optional whitespace.
--
-- A common separator in Operators and Lambdas.
tokNSpaced :: Scanner String
tokNSpaced = tokSpace <|> tokNewLine >> whitespace
  where
    tokNewLine = char '\n'



-- | Parse a single character, escaping it if
-- it is preceded by a backslash and has no literal
-- meaning.
withCharEscape :: Bool -- ^ Treat backslashes as literal backslashes.
        -> Scanner String
withCharEscape b = do
  c <- anyChar
  case c of
    '\\' -> if b
            then liftM (\x -> [c,x]) anyChar
            else escString -- liftM (:[]) escChar
                 <|> liftM (\x -> [c, x]) anyChar
    _    -> return [c]


escString :: Scanner String
escString = (escEmpty >> return "")
            <|> liftM (:[]) escChar


escEmpty :: Scanner Char
escEmpty = char '&'
           <|> (some tokSpace >> char '\\')


escChar :: Scanner Char
escChar = genEsc
          <|> escNum
          <|> asciiEsc
          <|> controlEsc
          <?> "escape code"


controlEsc :: Scanner Char
controlEsc = do
  char '^'
  code <- upper
  return $ toEnum $ fromEnum code - fromEnum 'A'
    where upper = cond isUpper


toBase :: (Show a, Integral a) => a -> a -> String
toBase base num = showIntAtBase base intToDigit num ""


fromBase :: Int -> String -> Int
fromBase base = fst . head . readInt base ((<base) . digitToInt) digitToInt


numBase :: Int -> Scanner Char -> Scanner Int
numBase base baseDig = do
  s <- some baseDig
  return $ read $ toBase10 $ fromBase base s
  where toBase10 = toBase 10


escNum :: Scanner Char
escNum = do
  code <- numBase 10 tokDenaryDigit
          <|> (char 'o' >> numBase 8 octDigit)
          <|> (char 'x' >> numBase 16 hexDigit)
  return $ toEnum $ fromIntegral code
      where hexDigit = cond isHexDigit
            octDigit = cond isOctDigit


genEsc :: Scanner Char
genEsc = choice (map genEscChar escs)
    where genEscChar c = char c >> return (codeToChar [c])
          escs = "abfnrtv\\\"\'"


asciiEsc :: Scanner Char
asciiEsc = choice (map asciiEscChar asciis)
    where asciiEscChar asc = tryScan (string asc) >> return (codeToChar asc)
          asciis = ascii3codes ++ ascii2codes


ascii2codes :: [String]
ascii2codes = [ "BS","HT","LF","VT","FF","CR","SO","SI","EM"
              , "FS","GS","RS","US","SP"]


ascii3codes :: [String]
ascii3codes = [ "NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL"
              , "DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB"
              , "CAN","SUB","ESC","DEL"]


codeToChar :: String -> Char
codeToChar s = case readLitChar ('\\':s) of
                 [(r,"")] -> r
                 _       -> error $ "codeToChar: not a valid code: " ++ s
