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
    , tokTrue, tokFalse
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
    ) where
    

import Angle.Lex.Helpers
import Control.Applicative
import Control.Monad
import Data.Char (isDigit, isSpace, isAlpha, isAlphaNum)

tokStmtEnd        = char ';' <|> char '\n' 
                                    <?> "end of statement"
tokListStart      = char '['        <?> "start of list"
tokListEnd        = char ']'        <?> "end of list"
tokParenL         = char '('        <?> "open parenthesis"
tokParenR         = char ')'        <?> "close parenthesis"
tokColon          = char ':'        <?> "colon"
tokMultiStmtStart = surrounded whitespace (char '{') 
                                    <?> "start of multi-statement"
tokMultiStmtEnd   = surrounded whitespace (char '}') 
                                    <?> "end of multi-statement"
tokEltSep         = surrounded whitespace (char ',')
                                    <?> "element separator"
tokIdentStartChar = cond isAlpha
                                    <?> "start of identifier"
tokIdentBodyChar  = cond isAlphaNum
                                    <?> "identifier character"
tokStringStart    = char '"'        <?> "start of string"
tokStringEnd      = char '"'        <?> "end of string"
tokStringBodyChar = notChar '"'     <?> "string body"
tokDenaryDigit    = cond isDigit    <?> "denary digit"
tokTupleStart     = tokParenL       <?> "start of tuple"
tokTupleEnd       = tokParenR       <?> "end of tuple"
tokSpace          = char ' '        <?> "space"
-- Might be an issue with this
tokWhitespace     = cond isSpace    <?> "whitespace"
tokAssign         = surrounded spaces (char '=') 
                                    <?> "assignment operator"
tokRangeSep       = string ".."     <?> "range separator"
tokTrue           = string "true"   <?> "true"
tokFalse          = string "false"  <?> "false"
tokPeriod         = char '.'        <?> "period"
tokNewLine        = char '\n'       <?> "newline"
tokEOF            = notScan anyChar -- <?> "eof"
tokStmtBetween    = whitespace      <?> "ignored characters"
         
tokInt :: (Integral a, Read a) => Scanner a
tokInt = do
  negve <- optional (char '-')
  res <- read <$> some tokDenaryDigit <?> "integer"
  case negve of
    Nothing -> return res
    Just _ -> return (-res)
         
tokDigits :: Scanner String
tokDigits = some tokDenaryDigit

tokFloat :: Scanner Float
tokFloat = read <$> chainFlat [tokDigits, string ".", tokDigits]

tokList :: Scanner a -> Scanner a
tokList = within tokListStart tokListEnd

-- |Function/variable identifier (but not a keyword)
-- >>> evalScan "test" ident
-- Right "test"
--
-- >>> evalScan "return" ident
-- Left ...
-- ...
ident = noneFrom string keywords *> ((:) <$> tokIdentStartChar <*> many tokIdentBodyChar)

opChars = "*/+->=<|&^"
sepChar = "{()};, =" ++ opChars

exprEnd = lookAhead $ (charFrom "});,")
          
exprSep = lookAhead (charFrom sepChar) <?> "expression boundary"
                
-- TODO: Add built-in functions
keywords = [ "defun"
           , "do"
           , "else"
           , "false"
           , "for"
           , "if"
           , "in"
           , "return"
           , "then"
           , "true"
           , "while"]
           
-- TODO: Is this needed?
keyword str = string str <* tokSpace

-- |Run scan within parentheses
-- >>> evalScan "(test)" (parens (string "test"))
-- Right "test"
--
-- >>> evalScan "test" (parens (string "test"))
-- Left ...
-- ...
parens :: Scanner a -> Scanner a
parens sc = within tokParenL tokParenR sc 
            <?> "parentheses"
         
tokString = within tokStringStart tokStringEnd 
            (many tokStringBodyChar) <?> "string"

tuple sc = within tokTupleStart tokTupleEnd 
           (sepWith tokEltSep sc) <?> "tuple"

checkStmtEnd = lookAhead tokMultiStmtEnd <|> tokStmtEnd
stmtEnd = tokMultiStmtEnd <|> tokStmtEnd

               
whitespace = many tokWhitespace

spaces = many tokSpace
