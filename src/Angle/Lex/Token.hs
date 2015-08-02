module Angle.Lex.Token
    ( tokStmtEnd 
    , tokListStart 
    , tokListEnd 
    , tokParenL 
    , tokParenR 
    , tokColon 
    , tokMultiStmtStart
    , tokMultiStmtEnd 
    , tokEltSep 
    , tokStringStart
    , tokStringEnd
    , tokStringBodyChar
    , tokDenaryDigit
    , tokTupleStart
    , tokTupleEnd
    , tokAssign
    , tokRangeSep
    , tokSpace
    , tokWhitespace
    , tokNSpaced
    , tokPeriod
    , tokString
    , tokNewLine
    , tokEOF
    , tokStmtBetween
    , tokList
    , tokFloat
    , tokInt
    , ident
    , parens
    , keyword
    , exprSep
    , exprEnd
    , checkStmtEnd
    , whitespace
    , spaces
    , keywords
    , tokFunRefStart
    , tokOpChar
    , tuple
    ) where
    

import Control.Applicative
import Control.Monad
import Data.Char (isDigit, isSpace, isAlpha, isAlphaNum)

import Angle.Lex.Helpers


tokStmtEnd :: Scanner Char
tokStmtEnd        = char ';' <|> char '\n' 


                                    <?> "end of statement"
tokListStart :: Scanner Char
tokListStart      = char '['        <?> "start of list"


tokListEnd :: Scanner Char
tokListEnd        = char ']'        <?> "end of list"


tokParenL :: Scanner Char
tokParenL         = char '('        <?> "open parenthesis"


tokParenR :: Scanner Char
tokParenR         = char ')'        <?> "close parenthesis"


tokColon :: Scanner Char
tokColon          = char ':'        <?> "colon"


tokMultiStmtStart :: Scanner Char
tokMultiStmtStart = surrounded whitespace (char '{') 
                                    <?> "start of multi-statement"


tokMultiStmtEnd :: Scanner Char
tokMultiStmtEnd   = surrounded whitespace (char '}') 
                                    <?> "end of multi-statement"


tokEltSep :: Scanner Char
tokEltSep         = surrounded whitespace (char ',')
                                    <?> "element separator"


tokIdentStartChar :: Scanner Char
tokIdentStartChar = cond isAlpha
                                    <?> "start of identifier"


tokIdentBodyChar :: Scanner Char
tokIdentBodyChar  = cond isAlphaNum
                                    <?> "identifier character"


tokStringStart :: Scanner Char
tokStringStart    = char '"'        <?> "start of string"


tokStringEnd :: Scanner Char
tokStringEnd      = char '"'        <?> "end of string"


tokStringBodyChar :: Scanner Char
tokStringBodyChar = notChar '"'     <?> "string body"


tokDenaryDigit :: Scanner Char
tokDenaryDigit    = cond isDigit    <?> "denary digit"


tokTupleStart :: Scanner Char
tokTupleStart     = tokParenL       <?> "start of tuple"


tokTupleEnd :: Scanner Char
tokTupleEnd       = tokParenR       <?> "end of tuple"


tokSpace :: Scanner Char
tokSpace          = char ' '        <?> "space"


-- Might be an issue with this
tokWhitespace :: Scanner Char
tokWhitespace     = cond isSpace    <?> "whitespace"


tokAssign :: Scanner Char
tokAssign         = surrounded spaces (char '=') 
                                    <?> "assignment operator"


tokRangeSep :: Scanner String
tokRangeSep       = string ".."     <?> "range separator"


tokPeriod :: Scanner Char
tokPeriod         = char '.'        <?> "period"


tokNewLine :: Scanner Char
tokNewLine        = char '\n'       <?> "newline"


tokEOF :: Scanner ()
tokEOF            = notScan anyChar -- <?> "eof"


tokStmtBetween :: Scanner String
tokStmtBetween    = whitespace      <?> "ignored characters"


tokFunRefStart :: Scanner Char
tokFunRefStart = char '$' <?> "function reference"

         
tokInt :: (Integral a, Read a) => Scanner a
tokInt = do
  negve <- optional (char '-')
  res <- read <$> tokDigits <?> "integer"
  case negve of
    Nothing -> return res
    Just _ -> return (-res)
         

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
    Just _ ->  return (-res)


tokList :: Scanner a -> Scanner a
tokList = within tokListStart tokListEnd


-- | Function/variable identifier (but not a keyword).
ident :: Scanner String
ident = noneFrom (\x -> string x <* specEnd) keywords *> ((:) <$> tokIdentStartChar <*> many tokIdentBodyChar)
    where specEnd = notScan tokIdentBodyChar
        

opChars :: String
opChars = "*/+->=<|&^"

          
tokOpChar :: Scanner Char
tokOpChar = charFrom "*/+->=<|&^"


sepChar :: String
sepChar = "{()};, =" ++ opChars


exprEnd :: Scanner ()
exprEnd = lookAhead (void tokRangeSep <|> void tokStmtEnd <|> void tokEltSep <|> void tokParenR)

          
exprSep :: Scanner Char
exprSep = lookAhead (charFrom sepChar) <?> "expression boundary"

                
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
           

-- TODO: Is this needed?
keyword :: String -> Scanner String
keyword str = string str <* tokSpace


-- | Run scan within parentheses.
parens :: Scanner a -> Scanner a
parens sc = within tokParenL tokParenR sc 
            <?> "parentheses"
         

tokString :: Scanner String
tokString = within tokStringStart tokStringEnd 
            (many tokStringBodyChar) <?> "string"


tuple :: Scanner b -> Scanner [b]
tuple sc = within tokTupleStart tokTupleEnd 
           (sepWith tokEltSep sc) <?> "tuple"


checkStmtEnd :: Scanner Char
checkStmtEnd = lookAhead tokMultiStmtEnd <|> tokStmtEnd

               
whitespace :: Scanner String
whitespace = many tokWhitespace


spaces :: Scanner String
spaces = many tokSpace


tokNSpaced = tokSpace <|> tokNewLine >> whitespace
