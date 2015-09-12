{-|
Module      : Angle.Interpreter
Description : File runner for the Angle programming language.
Copyright   : Copyright (C) 2015 Ben Moon
License     : GNU GPL, version 3
Maintainer  : GuiltyDolphin@gmail.com
Stability   : alpha

Defines the file runner for use with the Angle programming language.
-}
module Angle.Interpreter (runInterpreter) where

import Control.Monad
import System.IO.Error
import System.Exit

import Angle.Options
import Angle.Exec.Error
import Angle.Scanner
import Angle.Exec.Exec
import Angle.Exec.Builtins
import Angle.Exec.Types
import Angle.Parse.Parser


runInterpreter :: Options -> IO ()
runInterpreter opts = do
    let Options { optFiles = files
                , optAbort = abort
                } = opts

    runFiles files abort


runFiles :: [FilePath] -> Bool -> IO ()
runFiles fs abort = mapM_ (`runFile` abort) fs


runFile :: FilePath -> Bool -> IO ()
runFile fp abort = do
    source' <- tryIOError $ readFile fp
    case source' of
        Left e -> handleNoFile fp abort e
        Right source -> runLex source
  where
    runLex source =
        case evalParse source program of
            Left err -> handleSyntax fp err abort
            Right toExec -> executeProg toExec
    executeProg toExec = do
        res <- runExecIOEnv initialEnvMain (execStmt toExec)
        case res of
            Left err -> handleRuntime fp err abort
            Right _ -> return ()

handleSyntax :: FilePath -> ParseError -> Bool -> IO ()
handleSyntax fp e abort = do
    putStrLn ("Syntax error in file " ++ fp) >> print e
    when abort $ exitWith $ ExitFailure 100


handleRuntime :: FilePath -> AngleError -> Bool -> IO ()
handleRuntime fp e abort = do
    putStrLn ("Runtime error in file " ++ fp) >> print e
    when abort $ exitWith $ ExitFailure 101


handleNoFile :: FilePath -> Bool -> IOError -> IO ()
handleNoFile fp abort e | isDoesNotExistError e = handleNoFile'
                        | otherwise = ioError e
  where
    handleNoFile' = do
        putStrLn $ "No such file: " ++ fp
        when abort $ exitWith $ ExitFailure 102

-- -- import System.IO
-- import Control.Monad.State
-- import System.Environment
-- import System.Exit
--
-- import Angle.Parse.Parser
-- import Angle.Exec.Exec
-- import Angle.Scanner (ParseError)
-- import Angle.Exec.Error
-- import Angle.Exec.Types
-- import Angle.Types.Lang
--
--
-- stringToStmt :: String -> Either ParseError Stmt
-- stringToStmt s = evalParse s program
--
--
-- runStmt :: String -> Stmt -> [String] -> IO (Either AngleError LangLit)
-- runStmt source s _ = do
--   --let env' = initializeEnv args
--   --    env = env' { sourceText=source }
--   let env = basicEnv { sourceText = source }
--   runExecIOEnv env (execStmt s)
--
--
-- runFile :: Bool -> FilePath -> [String] -> IO ()
-- runFile showSyntx f args = do
--   s <- readFile f
--   case stringToStmt s of
--     Left err -> putStrLn "Syntax error" >> print err >> exitWith (ExitFailure 100)
--     Right r -> do
--               when showSyntx (do
--                   putStrLn "Here's what I think..."
--                   putStrLn (showSyn r))
--               res <- runStmt s r args
--               case res of
--                 Left err -> putStrLn "Runtime error" >> print err >> exitWith (ExitFailure 101)
--                 Right _ -> return ()
--
--
--
-- interp :: Options -> IO ()
-- interp (Options { showRep=sr, fileName=f, progArgs=args }) = runFile sr f args
--
--
-- data Options = Options { showRep :: Bool
--                        , fileName :: FilePath
--                        , progArgs :: [String]}
--                deriving (Show, Eq)
--
--
-- basicOptions :: Options
-- basicOptions = Options { showRep = False
--                        , fileName = ""
--                        , progArgs = [] }
--
--
-- argsToOpts :: [String] -> Options
-- argsToOpts xs | "--show-rep" `elem` xs = (argsToOpts xs') { showRep = True }
--               where xs' = filter (/="--show-rep") xs
-- argsToOpts args@(f:_) = basicOptions { fileName = f, progArgs = args }
-- argsToOpts _ = error "argsToOpts: invalid args"
-- -- argsToOpts [f] = basicOptions { fileName = f }
--
-- main :: IO ()
-- main = liftM argsToOpts getArgs >>= interp
--
--
-- interpWithArgs :: [String] -> IO ()
-- interpWithArgs = interp . argsToOpts
