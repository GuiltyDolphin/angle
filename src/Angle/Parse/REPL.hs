module Angle.Parse.REPL 
    ( main 
    ) where
    
import Control.Monad
import Control.Monad.State
import Control.Monad.Error

import Angle.Lex.Lexer
import Angle.Lex.Helpers
import Angle.Parse.Exec
import Angle.Parse.Error
import Angle.Types.Lang

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
                toPrint <- execStmt res `catchError` (\e -> liftIO (print e) >> return LitNull)
                liftIO $ print toPrint


mainProg = do
  liftIO $ putStr "> "
  userInput <- liftIO getLine
  st <- get
  put st { sourceText=userInput}
  unless (userInput=="exit")
           (do 
             runLine userInput
             mainProg)
  
  
main = runExecIOBasic mainProg
  -- putStr "> "
  -- userInput <- getLine
  -- unless (userInput=="exit") (processLine userInput >> main)

       

-- Ideas for Error tracking:
-- - Expansion-based error finder
--    when it encounters an error in a statement, requests the
--    lexer to re-parse a small section of code between the
--    statement boundaries and asks for smaller and smaller
--    chunks until the position of the bad token is resolved.
--    (resolvePosition :: Scanner a -> String -> (SourcePos, SourcePos))
