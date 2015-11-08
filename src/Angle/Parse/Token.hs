{-|
Module      : Angle.Parse.Token
Description : Definitons for language tokens.
Copyright   : Copyright (C) 2015 Ben Moon
License     : GNU GPL, version 3
Maintainer  : GuiltyDolphin@gmail.com
Stability   : alpha

Definitions for basic parsers to be used by the main parser.
-}
module Angle.Parse.Token
    (
    -- ** Whitespace
      tokNSpaced
    , tokSpace
    , tokWhitespace
    , whitespace

    -- ** Statements
    , tokAssign
    , tokAssignNonLocal
    , tokStmtBetween
    , tokMultiStmtEnd
    , tokMultiStmtStart

    -- ** Literals
    , tokFloat
    , tokInt
    , tokList
    , tokChar

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

import Angle.Parse.Helpers


-- | Start of a multi-stmt.
tokMultiStmtStart :: Parser Char
tokMultiStmtStart = surrounded whitespace (char '{')
                                    <?> "start of multi-statement"


-- | End of a multi-stmt.
tokMultiStmtEnd :: Parser Char
tokMultiStmtEnd   = surrounded whitespace (char '}')
                                    <?> "end of multi-statement"


-- | Element separator for lists and arguments.
tokEltSep :: Parser Char
tokEltSep         = surrounded whitespace (char ',')
                                    <?> "element separator"


tokDenaryDigit :: Parser Char
tokDenaryDigit    = cond isDigit    <?> "denary digit"


-- | A single space character.
tokSpace :: Parser Char
tokSpace          = char ' '        <?> "space"


-- | A single space character (see 'isSpace' for what qualifies).
tokWhitespace :: Parser Char
tokWhitespace     = cond isSpace    <?> "whitespace"


-- | Assignment operator character.
tokAssign :: Parser Char
tokAssign = surrounded whitespace (char '=')
                <?> "assignment operator"

tokAssignNonLocal :: Parser String
tokAssignNonLocal = surrounded whitespace (string "|=")
                    <?> "nonlocal assignment"


-- | Matches any amount of whitespace.
whitespace :: Parser String
whitespace = many tokWhitespace


-- | Matches when there are no more characters in the stream.
tokEOF :: Parser ()
tokEOF            = notParse anyChar


-- | Characters valid between statements.
tokStmtBetween :: Parser String
tokStmtBetween    = whitespace      <?> "ignored characters"


-- | Matches a string representing an integer.
tokInt :: (Integral a, Read a) => Parser a
tokInt = do
  negve <- optional (char '-')
  res <- read <$> tokDigits <?> "integer"
  case negve of
    Nothing -> return res
    Just _  -> return (-res)


tokDigits :: Parser String
tokDigits = some tokDenaryDigit


-- | Matches a string representing a floating-point number.
tokFloat :: Parser Double
tokFloat = do
  negve <- optional (char '-')
  f <- tokDigits
  rst <- (:) <$> char '.' <*> tokDigits
  let res = read $ f ++ rst
  case negve of
    Nothing -> return res
    Just _  ->  return (-res)


-- | Matches within square brackets.
tokList :: Parser a -> Parser a
tokList = within tokStartList tokEndList
  where
    tokStartList = char '[' <* whitespace
    tokEndList = whitespace *> char ']'


-- | Function/variable identifier (but not a keyword).
ident ::
    Bool -- ^Are keywords allowed?
    -> Parser String
ident b = unless b (noneFrom (\x -> string x <* specEnd) keywords) *> ((:) <$> tokIdentStartChar <*> many tokIdentBodyChar)
    where specEnd = notParse tokIdentBodyChar
          tokIdentStartChar = cond (\x -> isAlpha x || x == '_')
          tokIdentBodyChar  = cond (\x -> isAlphaNum x || x == '_')


-- | Valid operator character.
tokOpChar :: Parser Char
tokOpChar = charFrom "*/+->=<|&^"


-- | Angle keywords.
keywords :: [String]
keywords = [ "break"
           , "catch"
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
           , "raise"
           , "return"
           , "then"
           , "true"
           , "try"
           , "unless"
           , "when"
           , "while"]


-- | Matches within parentheses.
parens :: Parser a -> Parser a
parens sc = within tokParenL tokParenR sc
            <?> "parentheses"
  where
    tokParenL = char '(' <* whitespace
    tokParenR = whitespace *> char ')'


stringNorm :: Parser String
stringNorm = do
  char '"'
  r <- manyTill (char '"') (withCharEscape False)
  char '"'
  return (concat r)


-- | String of the form "BODY".
tokString :: Parser String
tokString = tryParse stringBS <|> stringNorm


-- | Character of the form 'C'.
tokChar :: Parser Char
tokChar = surrounded (char '\'') charNonEmpty
  where
    charNonEmpty = do
      c <- notChar '\'' -- anyChar
      case c of
        '\\' -> escChar
        _ -> return c


stringBS :: Parser String
stringBS = do
  string "e\""
  r <- manyTill (char '"') (withCharEscape True)
  char '"'
  return (concat r)


-- | Space or newline followed by optional whitespace.
--
-- A common separator in Operators and Lambdas.
tokNSpaced :: Parser String
tokNSpaced = tokSpace <|> tokNewLine >> whitespace
  where
    tokNewLine = char '\n'



-- | Parse a single character, escaping it if
-- it is preceded by a backslash and has no literal
-- meaning.
withCharEscape :: Bool -- ^ Treat backslashes as literal backslashes.
        -> Parser String
withCharEscape b = do
  c <- anyChar
  case c of
    '\\' -> if b
            then liftM (\x -> [c,x]) anyChar
            else escString -- liftM (:[]) escChar
                 <|> liftM (\x -> [c, x]) anyChar
    _    -> return [c]


escString :: Parser String
escString = (escEmpty >> return "")
            <|> liftM (:[]) escChar


escEmpty :: Parser Char
escEmpty = char '&'
           <|> (some tokSpace >> char '\\')


escChar :: Parser Char
escChar = genEsc
          <|> escNum
          <|> asciiEsc
          <|> controlEsc
          <?> "escape code"


controlEsc :: Parser Char
controlEsc = do
  char '^'
  code <- upper
  return $ toEnum $ fromEnum code - fromEnum 'A'
    where upper = cond isUpper


toBase :: (Show a, Integral a) => a -> a -> String
toBase base num = showIntAtBase base intToDigit num ""


fromBase :: Int -> String -> Int
fromBase base = fst . head . readInt base ((<base) . digitToInt) digitToInt


numBase :: Int -> Parser Char -> Parser Int
numBase base baseDig = do
  s <- some baseDig
  return $ read $ toBase10 $ fromBase base s
  where toBase10 = toBase 10


escNum :: Parser Char
escNum = do
  code <- numBase 10 tokDenaryDigit
          <|> (char 'o' >> numBase 8 octDigit)
          <|> (char 'x' >> numBase 16 hexDigit)
  return $ toEnum $ fromIntegral code
      where hexDigit = cond isHexDigit
            octDigit = cond isOctDigit


genEsc :: Parser Char
genEsc = choice (map genEscChar escs)
    where genEscChar c = char c >> return (codeToChar [c])
          escs = "abfnrtv\\\"\'"


asciiEsc :: Parser Char
asciiEsc = choice (map asciiEscChar asciis)
    where asciiEscChar asc = tryParse (string asc) >> return (codeToChar asc)
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
