module Angle.Parse.REPL where

import Angle.Lex.Lexer
import Angle.Lex.Helpers
import Angle.Parse.Parser

processLine :: String -> IO ()
processLine s = case evalScan s program of
                  Left err -> print err
                  Right res -> mapM (print . evalStmt) res

main = do
  putStr "> "
  userInput <- getStrLn
  unless (userInput=="exit") (processLine userInput >> main)
