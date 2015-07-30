module Angle.Parse.Interp
    ( main
    ) where

-- import System.IO
import Control.Monad
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
                 
runStmt :: String -> Stmt -> IO (Either LError LangLit)
runStmt source s = runExecIOEnv (basicEnv {sourceText=source}) (execStmt s)
            
runFile :: Bool -> FilePath -> IO ()
runFile showSyntx f = do
  s <- readFile f
  case stringToStmt s of
    Left err -> putStrLn "Syntax error" >> print err >> exitWith (ExitFailure 100)
    Right r -> do
              when showSyntx (do
                  putStrLn "Here's what I think..."
                  putStrLn (showSyn r))
              res <- runStmt s r
              case res of
                Left err -> putStrLn "Runtime error" >> print err >> exitWith (ExitFailure 101)
                Right _ -> return ()
                             
interp :: Options -> IO ()
interp (Options { showRep=sr, fileName=f }) = runFile sr f
               

data Options = Options { showRep :: Bool
                       , fileName :: FilePath }
               deriving (Show, Eq)
                        

basicOptions :: Options
basicOptions = Options { showRep = False
                       , fileName = "" }


argsToOpts :: [String] -> Options
argsToOpts xs | "--show-rep" `elem` xs = (argsToOpts xs') { showRep = True }
              where xs' = filter (/="--show-rep") xs
argsToOpts [f] = basicOptions { fileName = f }

main :: IO ()
main = liftM argsToOpts getArgs >>= interp
