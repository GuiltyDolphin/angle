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
    , ident
    , parens
    , keyword
    , exprSep
    , checkStmtEnd
    , whitespace
    , spaces
    ) where
    

import Angle.Lex.Helpers
import Control.Applicative
import Data.Char (isDigit, isSpace)

tokStmtEnd = char ';' <|> char '\n' <?> "end of statement"
tokListStart = char '[' <?> "start of list"
tokListEnd = char ']' <?> "end of list"
tokParenL = char '(' <?> "open parenthesis"
tokParenR = char ')' <?> "close parenthesis"
tokColon = char ':' <?> "colon"
tokMultiStmtStart = surrounded whitespace (char '{') <?> "start of multi-statement"
tokMultiStmtEnd = surrounded whitespace (char '}') <?> "end of multi-statement"
tokEltSep = char ',' <* whitespace <?> "element separator"
tokIdentStartChar = cond (`notElem` reservedChars) <?> "start of identifier"
tokIdentBodyChar = tokIdentStartChar <?> "identifier character"
tokStringStart = char '"' <?> "start of string"
tokStringEnd = char '"' <?> "end of string"
tokStringBodyChar = notChar '"' <?> "string body"
tokDenaryDigit = cond isDigit <?> "denary digit"
-- TODO: Think of a better (and more relevant) 
-- name than `declaration'
tokTupleStart = tokParenL <?> "start of tuple"
tokTupleEnd = tokParenR <?> "end of tuple"
tokSpace = char ' ' <?> "space"
-- Might be an issue with this
tokWhitespace = cond isSpace <?> "whitespace"
tokAssign = surrounded spaces (char '=') <?> "assignment operator"
tokRangeSep = string ".." <?> "range separator"
tokTrue = string "true" <?> "true"
tokFalse = string "false" <?> "false"
tokPeriod = char '.' <?> "period"
tokNewLine = char '\n' <?> "newline"
tokEOF = notScan anyChar <?> "eof"
tokStmtBetween = whitespace <?> "ignored characters"
         
tokInt = some tokDenaryDigit <?> "integer"

tokFloat :: Scanner Float
tokFloat = do
  f <- tokInt
  tokPeriod
  rest <- tokInt
  return $ read (f ++ "." ++ rest)

-- prop> \xs -> not $ any (`notElem` reservedChars) xs
-- prop> \xs -> evalScan xs ident
-- ident = do 
--   noneFrom string keywords
--   (:) <$> tokIdentStartChar <*> many tokIdentBodyChar <?> "identifier"                 
--         

-- |Function/variable identifier (but not a keyword)
-- >>> evalScan "test" ident
-- Right "test"
--
-- >>> evalScan "return" ident
-- Left ...
-- ...
ident = do
  noneFrom string keywords
  (:) <$> alpha <*> many (alpha <|> digit)

reservedChars = "<>;\n:{}\"'$@, "    
                
opChars = "*/+->=<|&^"
sepChar = "{()};, =" ++ opChars
          
          
exprSep = lookAhead (charFrom sepChar) <?> "expression boundary"
                
alpha = charFrom ['A'..'z']
digit = charFrom ['0'..'9']

keywords = ["defun", "do", "else", "false", "for", "if", "in", "return", "then", "true", "while"]
           
keyword str = string str <* tokSpace

-- |Run scan within parentheses
-- >>> evalScan "(test)" (parens (string "test"))
-- Right "test"
--
-- >>> evalScan "test" (parens (string "test"))
-- Left ...
-- ...
parens :: Scanner a -> Scanner a
parens sc = within tokParenL tokParenR sc <?> "parentheses"
         
tokList = within tokListStart tokListEnd

tokString = within tokStringStart tokStringEnd (many tokStringBodyChar) <?> "string"

tuple sc = within tokTupleStart tokTupleEnd (sepWith tokEltSep sc)

checkStmtEnd = lookAhead tokMultiStmtEnd <|> tokStmtEnd

               
whitespace = many tokWhitespace

spaces = many tokSpace
