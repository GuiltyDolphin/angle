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

    -- ** Statements
    , tokAssign
    , tokAssignNonLocal
    , tokAssignGlobal
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
    -- ** Identifiers
    , ident
    , validSymbolIdentChars
    , builtinOps
    , brackets
    , comma
    , parens
    , symbol
    , tokOpChar
    , tokEltSep
    ) where


import Control.Applicative ((<*), (<*>), (*>), (<$>))
import Control.Monad
import Data.Char
import Numeric

import Angle.Parse.Helpers

import Text.Parsec

import qualified Text.Parsec.Token as P
import Text.Parsec.Language


angleLexer = P.makeTokenParser emptyDef
  { P.commentLine = "#" }

comma :: Parser st String
comma = P.comma angleLexer

-- | Start of a multi-stmt.
tokMultiStmtStart :: Parser st Char
tokMultiStmtStart = surrounded spaces (char '{')
                                    <?> "start of multi-statement"


-- | End of a multi-stmt.
tokMultiStmtEnd :: Parser st Char
tokMultiStmtEnd   = surrounded spaces (char '}')
                                    <?> "end of multi-statement"


-- | Element separator for lists and arguments.
tokEltSep :: Parser st Char
tokEltSep         = surrounded spaces (char ',')
                                    <?> "element separator"


tokDenaryDigit :: Parser st Char
tokDenaryDigit    = satisfy isDigit    <?> "denary digit"


-- | A single space character.
tokSpace :: Parser st Char
tokSpace          = char ' '        <?> "space"


-- | Assignment operator character.
tokAssign :: Parser st Char
tokAssign = surrounded spaces (char '=')
                <?> "assignment operator"

tokAssignNonLocal :: Parser st String
tokAssignNonLocal = surrounded spaces (string "|=")
                    <?> "nonlocal assignment"


tokAssignGlobal :: Parser st String
tokAssignGlobal = surrounded spaces (string "||=")
                    <?> "global assignment"


-- | Matches when there are no more characters in the stream.
tokEOF :: Parser st ()
tokEOF            = notFollowedBy anyChar


-- | Characters valid between statements.
tokStmtBetween :: Parser st ()
tokStmtBetween    = spaces      <?> "ignored characters"


-- | Matches a string representing an integer.
tokInt :: (Integral a, Read a) => Parser st a
tokInt = do
  negve <- optionMaybe (char '-')
  res <- read <$> tokDigits <?> "integer"
  case negve of
    Nothing -> return res
    Just _  -> return (-res)


tokDigits :: Parser st String
tokDigits = many1 tokDenaryDigit


-- | Matches a string representing a floating-point number.
tokFloat :: Parser st Double
tokFloat = do
  negve <- optionMaybe (char '-')
  f <- tokDigits
  rst <- (:) <$> char '.' <*> tokDigits
  let res = read $ f ++ rst
  case negve of
    Nothing -> return res
    Just _  ->  return (-res)


-- | Matches within square brackets.
tokList :: Parser st a -> Parser st a
tokList = between tokStartList tokEndList
  where
    tokStartList = char '[' <* spaces
    tokEndList = spaces *> char ']'


-- | Function/variable identifier (but not a keyword).
ident ::
    Bool -- ^Are keywords allowed?
    -> Parser st String
ident b = (symbolIdent <|> namedIdent b) <?> "identifier"

namedIdent :: Bool -> Parser st String
namedIdent b = try (unless b (noneFrom (\x -> string x <* specEnd) keywords) *> ((:) <$> tokIdentStartChar <*> many tokIdentBodyChar))
    where specEnd = notFollowedBy tokIdentBodyChar
          tokIdentStartChar = satisfy (\x -> isAlpha x || x == '_')
          tokIdentBodyChar  = satisfy (\x -> isAlphaNum x || x == '_')

symbolIdent :: Parser st String
symbolIdent = try (many1 tokIdentBodyChar)
  where tokIdentBodyChar = satisfy (`elem` validSymbolIdentChars)


-- | Valid operator character.
tokOpChar :: Parser st Char
tokOpChar = oneOf "*/+->=<|&^"


builtinOps :: [String]
builtinOps = [ "+", "&", "++", "/", "==", ">", ">=", "<", "<="
             , "*", "^", "|", "-"]


-- | Angle keywords.
keywords :: [String]
keywords = [ "catch"
           , "defun"
           , "else"
           , "false"
           , "if"
           , "null"
           , "raise"
           , "then"
           , "true"
           , "try"
           , "unless"
           ]

validSymbolIdentChars :: String
validSymbolIdentChars = "*-+<>=/^|&"


-- | Matches within parentheses.
parens :: Parser st a -> Parser st a
parens sc = between tokParenL tokParenR sc
            <?> "parentheses"
  where
    tokParenL = char '(' <* spaces
    tokParenR = spaces *> char ')'


brackets :: Parser st a -> Parser st a
brackets = between (char '[' <* spaces) (spaces *> char ']')


symbol :: String -> Parser st String
symbol = P.symbol angleLexer


stringNorm :: Parser st String
stringNorm = do
  char '"'
  r <- manyTill (withCharEscape False) (char '"')
  return (concat r)


-- | String of the form "BODY".
tokString :: Parser st String
tokString = try stringBS <|> stringNorm


-- | Character of the form 'C'.
tokChar :: Parser st Char
tokChar = surrounded (char '\'') charNonEmpty
  where
    charNonEmpty = do
      c <- notChar '\''
      case c of
        '\\' -> escChar
        _ -> return c


stringBS :: Parser st String
stringBS = do
  string "e\""
  r <- manyTill (withCharEscape True) (char '"')
  return (concat r)


-- | Space or newline followed by optional whitespace.
--
-- A common separator in Operators and Lambdas.
tokNSpaced :: Parser st ()
tokNSpaced = (tokSpace <|> tokNewLine) >> spaces
  where
    tokNewLine = char '\n'

-- | Parse a single character, escaping it if
-- it is preceded by a backslash and has no literal
-- meaning.
withCharEscape :: Bool -- ^ Treat backslashes as literal backslashes.
        -> Parser st String
withCharEscape b = do
  c <- anyChar
  case c of
    '\\' -> if b
            then liftM (\x -> [c,x]) anyChar
            else escString -- liftM (:[]) escChar
                 <|> liftM (\x -> [c, x]) anyChar
    _    -> return [c]


escString :: Parser st String
escString = (escEmpty >> return "")
            <|> liftM (:[]) escChar


escEmpty :: Parser st Char
escEmpty = char '&'
           <|> (many1 tokSpace >> char '\\')


escChar :: Parser st Char
escChar = genEsc
          <|> escNum
          <|> asciiEsc
          <|> controlEsc
          <?> "escape code"


controlEsc :: Parser st Char
controlEsc = do
  char '^'
  code <- upper
  return $ toEnum $ fromEnum code - fromEnum 'A'
    where upper = satisfy isUpper


toBase :: (Show a, Integral a) => a -> a -> String
toBase base num = showIntAtBase base intToDigit num ""


fromBase :: Int -> String -> Int
fromBase base = fst . head . readInt base ((<base) . digitToInt) digitToInt


numBase :: Int -> Parser st Char -> Parser st Int
numBase base baseDig = do
  s <- many1 baseDig
  return $ read $ toBase10 $ fromBase base s
  where toBase10 = toBase 10


escNum :: Parser st Char
escNum = do
  code <- numBase 10 tokDenaryDigit
          <|> (char 'o' >> numBase 8 octDigit)
          <|> (char 'x' >> numBase 16 hexDigit)
  return $ toEnum $ fromIntegral code
      where hexDigit = satisfy isHexDigit
            octDigit = satisfy isOctDigit


genEsc :: Parser st Char
genEsc = choice (map genEscChar escs)
    where genEscChar c = char c >> return (codeToChar [c])
          escs = "abfnrtv\\\"\'"


asciiEsc :: Parser st Char
asciiEsc = choice (map asciiEscChar asciis)
    where asciiEscChar asc = try (string asc) >> return (codeToChar asc)
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
