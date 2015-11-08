module Angle.Lex.Token
    ( tokFunStart
    , tokFunEnd 
    , tokStmtEnd 
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
    , tokDeclareStart
    , tokDeclareEnd
    , tokSpecialStart
    , ident
    , angles
    , parens
    ) where
    

import Angle.Lex.Helpers
import Control.Applicative
import Data.Char (isDigit)

-- TODO: This doesn't actually do anything.
data Token = TokFunStart
           | TokFunEnd
           | TokStmtEnd
           | TokListStart
           | TokListEnd
           | TokParenL
           | TokParenR
           | TokColon
           | TokMultiStmtStart
           | TokMultiStmtEnd
           | TokEltSep
           | TokIdentStartChar
           | TokIdentBodyChar
           | TokStringStart
           | TokStringEnd
           | TokStringBody
           | TokDenaryDigit
           | TokPeriod
           | TokDeclare
             

tokFunStart = char '<' <?> "start of function"
tokFunEnd = char '>' <?> "end of function"
tokStmtEnd = char ';' <|> char '\n' <?> "end of statement"
tokListStart = char '[' <?> "start of list"
tokListEnd = char ']' <?> "end of list"
tokParenL = char '(' <?> "open parenthesis"
tokParenR = char ')' <?> "close parenthesis"
tokColon = char ':' <?> "colon"
tokMultiStmtStart = char '{' <?> "start of multi-statement"
tokMultiStmtEnd = char '}' <?> "end of multi-statement"
tokEltSep = char ',' <?> "element separator"
tokIdentStartChar = cond (`notElem` reservedChars) <?> "start of identifier"
tokIdentBodyChar = tokIdentStartChar <?> "identifier character"
tokStringStart = char '"' <?> "start of string"
tokStringEnd = char '"' <?> "end of string"
tokStringBodyChar = notChar '"' <?> "string body"
tokDenaryDigit = cond isDigit <?> "denary digit"
-- TODO: Think of a better (and more relevant) 
-- name than `declaration'
tokDeclareStart = char '@' <?> "start declaration"
tokDeclareEnd = char '@' <?> "end declaration"
tokSpecialStart = char '$' <?> "start of special value"

-- prop> \xs -> not $ any (`notElem` reservedChars) xs
ident = (:) <$> tokIdentStartChar <*> many tokIdentBodyChar <?> "identifier"                 
        
reservedChars = "<>;\n:{}\"'$@,"    
                

                
parens = within tokParenL tokParenR

-- |Characters within angle brackets
-- >>> evalScan "<test>" (angles . many $ notChar '>')
-- Right "test"
--
-- >>> evalScan "<test" (angles . many $ notChar '>')
-- Left ...
-- ...
angles = within (char '<') (char '>')
