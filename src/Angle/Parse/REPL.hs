module Angle.Parse.REPL where

import Angle.Lex.Lexer
import Angle.Lex.Helpers
import Angle.Parse.Exec
import Angle.Parse.Error

processLine :: String -> IO ()
processLine s = case evalScan s program of
                  Left err -> print err
                  Right res -> runExecIOBasic (execStmt res) >>= print
                               
-- execLine :: Env -> String -> IO Env
-- execLine e s = case evalScan s program of
--                  Left err -> print err
--                  Right res -> runExecIOEnv (execStmt e) res >>= print
                              
runLine :: String -> ExecIO ()
runLine s = case evalScan s program of
              Left err -> liftIO $ print err
              Right res -> do
                toPrint <- execStmt res
                liftIO $ print toPrint


mainProg = do
  liftIO $ putStr "> "
  userInput <- liftIO getLine
  unless (userInput=="exit")
           (do 
             runLine userInput
             mainProg)
  
  
main = runExecIOBasic mainProg
  -- putStr "> "
  -- userInput <- getLine
  -- unless (userInput=="exit") (processLine userInput >> main)
