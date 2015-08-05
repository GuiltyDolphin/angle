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
    , escString
    , bsString
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
import Data.Char --(isDigit, isSpace, isAlpha, isAlphaNum, readLitChar)
import Numeric

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

escString :: Scanner String
-- escString = surrounded (char '"') (liftM concat $ some stringChar) 
escString = do
  char '"'
  r <- manyTill (char '"') (escChar False)
  char '"'
  return (concat r)
         

bsString :: Scanner String
bsString = do
  string "e\""
  r <- manyTill (char '"') (escChar True)
  char '"'
  return (concat r)
  
  -- s <- tokString
  -- res <- mapM escChar s
  -- return $ concat res
  -- case reads ('"':s ++ "\"") of
  --   [] -> unexpectedErr "escString: Good grief!"
  --   [(res, "")] -> return res
  -- return $ read $ '"' : s ++ "\""


tuple :: Scanner b -> Scanner [b]
tuple sc = within tokTupleStart tokTupleEnd 
           (sepWith tokEltSep sc) <?> "tuple"


checkStmtEnd :: Scanner Char
checkStmtEnd = lookAhead tokMultiStmtEnd <|> tokStmtEnd

               
whitespace :: Scanner String
whitespace = many tokWhitespace


spaces :: Scanner String
spaces = many tokSpace

-- | Space or newline followed by optional whitespace.
--
-- A common separator in Operators and Lambdas.
tokNSpaced :: Scanner String
tokNSpaced = tokSpace <|> tokNewLine >> whitespace


-- | Parse a single character, escaping it if
-- it is preceded by a backslash and has no literal
-- meaning.
escChar :: Bool -- ^ Treat backslashes as literal backslashes.
        -> Scanner String
escChar b = do
  c <- anyChar
  case c of
    '\\' -> if b 
            then liftM (\x -> [c,x]) anyChar
            else stringEscape -- liftM (:[]) escapeCode
                 <|> liftM (\x -> [c, x]) anyChar
            -- n <- anyChar
            -- case readLitChar [c,n] of
            --   [] -> return [c,n]
            --   [(r,"")] -> return $ if b then [c,n] else [r]
    _ -> return [c]


characterChar :: Scanner Char
characterChar   = charLetter <|> charEscape
               <?> "literal character"

charEscape :: Scanner Char
charEscape      = char '\\' >> escapeCode

charLetter :: Scanner Char
charLetter = cond (\x -> (x /= '\\') && readLitChar [x] /= [])
-- charLetter      = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))

-- stringLiteral   = lexeme (
--                  do{ str <- between (char '"')
--                                     (char '"' <?> "end of string")
--                                     (many stringChar)
--                    ; return (foldr (maybe id (:)) "" str)
--                    }
--                  <?> "literal string")

stringChar = stringLetter <|> stringEscape
-- do{ c <- stringLetter; return (Just c) }
--                <|> stringEscape
--                <?> "string character"

stringLetter :: Scanner String
stringLetter    = liftM (:[]) $ cond (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

-- stringEscape    = do{ char '\\'
--                    ;     do{ escapeGap  ; return Nothing }
--                      <|> do{ escapeEmpty; return Nothing }
--                      <|> do{ esc <- escapeCode; return (Just esc) }
--                    }
stringEscape :: Scanner String
stringEscape = (escapeGap >> return "")
                <|> (escapeEmpty >> return "")
                <|> liftM (:[]) escapeCode


escapeEmpty :: Scanner Char
escapeEmpty     = char '&'

escapeGap :: Scanner Char
escapeGap = some tokSpace >> char '\\'
-- escapeGap       = do{ some tokSpace
--                    ; char '\\' <?> "end of string gap"
--                    }



-- escape codes
escapeCode :: Scanner Char
escapeCode = charEsc 
             <|> charNum 
             <|> charAscii 
             <|> charControl
             <?> "escape code"


upper :: Scanner Char
upper = cond isUpper

charControl :: Scanner Char
charControl = do
  char '^'
  code <- upper
  return $ toEnum $ fromEnum code - fromEnum 'A'
--charControl     = do{ char '^'
--                   ; code <- upper
--                   ; return (toEnum (fromEnum code - fromEnum 'A'))
--                   }


octDigit :: Scanner Char
octDigit = cond isOctDigit


hexDigit :: Scanner Char
hexDigit = cond isHexDigit
           

toBase :: (Show a, Integral a) => a -> a -> String
toBase base num = showIntAtBase base intToDigit num ""
                  

toBase10 :: Int -> String
toBase10 = toBase 10


fromBase :: Int -> String -> Int
fromBase base = fst . head . readInt base ((<base) . digitToInt) digitToInt
          

numBase :: Int -> Scanner Char -> Scanner Int
numBase base baseDig = do
  s <- some baseDig
  return $ read $ toBase10 $ fromBase base s


charNum :: Scanner Char
charNum = do
  code <- numBase 10 tokDenaryDigit
          <|> (char 'o' >> numBase 8 octDigit) 
          <|> (char 'x' >> numBase 16 hexDigit)
  return (toEnum (fromIntegral code))


charEsc :: Scanner Char
charEsc = choice (map parseEsc escMap)
    where parseEsc (c,code) = char c >> return code


charAscii :: Scanner Char
charAscii = choice (map parseAscii asciis) -- (map parseAscii asciiMap)
    where parseAscii asc = tryScan (string asc) >> return (codeToChar asc)
      --parseAscii (asc,code) = tryScan (string asc) >> return code

-- escape code tables

escMap :: [(Char, Char)]
escMap = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"
         
escs = "abfnrtv\\\"\'"


asciiMap :: [(String, Char)]
asciiMap = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)
           

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
                 [] -> error "codeToChar: not a valid code"



ascii2 :: String
ascii2 = [ '\BS','\HT','\LF','\VT','\FF','\CR','\SO','\SI'
         , '\EM','\FS','\GS','\RS','\US','\SP']


ascii3 :: String
ascii3 = [ '\NUL','\SOH','\STX','\ETX','\EOT','\ENQ','\ACK'
         , '\BEL','\DLE','\DC1','\DC2','\DC3','\DC4','\NAK'
         , '\SYN','\ETB','\CAN','\SUB','\ESC','\DEL']
