module Angle.Main.Prog where

import System.IO
import System.Environment
import Angle.Lex.Lexer (program)
import Angle.Lex.Helpers
import Angle.Parse.Parser (evalProg)
    

readProgram = (`evalScan` program)
              
main = do
  (fileName:_) <- getArgs
  source <- readFile fileName
  let lexRes = readProgram source
  case lexRes of
    Left x -> error ("failed in lexing\n" ++ show x)
    Right x -> do 
                let parseRes = evalProg x
                case parseRes of
                  Left x -> error ("failed in parsing\n" ++ x)
                  Right x -> putStrLn (show x)
                              
