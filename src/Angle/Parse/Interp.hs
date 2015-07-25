module Angle.Parse.Interp
    ( main
    ) where

-- import System.IO
import System.Environment
import System.Exit

import Angle.Lex.Lexer
import Angle.Parse.Exec
import Angle.Lex.Helpers (evalScan)
import Angle.Scanner (ScanError)
import Angle.Types.Lang
import Angle.Parse.Error
    
stringToStmt :: String -> Either ScanError Stmt
stringToStmt s = evalScan s program
                 
runStmt :: Stmt -> IO (Either LError LangLit)
runStmt s = runExecIOBasic (execStmt s)
            
runFile :: FilePath -> IO ()
runFile f = do
  s <- readFile f
  case stringToStmt s of
    Left err -> putStrLn "Syntax error" >> print err >> exitWith (ExitFailure 100)
    Right r -> do
              res <- runStmt r
              case res of
                Left err -> putStrLn "Runtime error" >> print err >> exitWith (ExitFailure 101)
                Right _ -> return ()
                             
interp :: [String] -> IO ()
interp (f:_) = runFile f

main :: IO ()
main = getArgs >>= interp
