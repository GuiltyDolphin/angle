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
import Angle.Exec.Builtins
import Angle.Exec.Error
import Angle.Exec.Exec
import Angle.Exec.Types
import Angle.Parse.Parser
import Angle.Scanner
import Angle.Types.Lang (Stmt)


runInterpreter :: Options -> IO ()
runInterpreter opts = do
    let Options { optFiles = files
                , optAbort = abort
                , optCode = code
                } = opts

    runFiles files abort
    runCodes code abort


runFiles :: [FilePath] -> Bool -> IO ()
runFiles fs abort = mapM_ (`runFile` abort) fs


runFile :: FilePath -> Bool -> IO ()
runFile fp abort = do
    source' <- tryIOError $ readFile fp
    case source' of
        Left e -> handleNoFile fp abort e
        Right source -> runLex (Just fp) source abort
  where
    -- runLex source =
    --     case evalParse source program of
    --         Left err -> handleSyntax (Just fp) err abort
    --         Right toExec -> executeProg toExec
    -- executeProg toExec = do
    --     res <- runExecIOEnv initialEnvMain (execStmt toExec)
    --     case res of
    --         Left err -> handleRuntime (Just fp) err abort
    --         Right _ -> return ()


runLex :: Maybe FilePath -> String -> Bool -> IO ()
runLex fp source abort =
    case evalParse source program of
        Left err -> handleSyntax fp err abort
        Right toExec -> executeProg fp source toExec abort


executeProg :: Maybe FilePath -> String -> Stmt -> Bool -> IO ()
executeProg fp source toExec abort = do
    res <- runExecIOEnv initialEnvMain { sourceText = source } (execStmt toExec)
    case res of
        Left err -> handleRuntime fp err abort
        Right _ -> return ()


runCodes :: [String] -> Bool -> IO ()
runCodes cs abort = mapM_ (`runCode` abort) cs


runCode :: String -> Bool -> IO ()
runCode = runLex Nothing


handleSyntax :: Maybe FilePath -> ParseError -> Bool -> IO ()
handleSyntax fp e abort = do
    putStrLn ("Syntax error" ++ fperror) >> print e
    -- putStrLn ("Syntax error in file " ++ fp) >> print e
    when abort $ exitWith $ ExitFailure 100
  where
    fperror = case fp of
                  (Just f) -> " in file " ++ f
                  Nothing -> ""


handleRuntime :: Maybe FilePath -> AngleError -> Bool -> IO ()
handleRuntime fp e abort = do
    putStrLn ("Runtime error" ++ fperror) >> print e
    -- putStrLn ("Runtime error in file " ++ fperror) >> print e
    when abort $ exitWith $ ExitFailure 101
  where
    fperror = case fp of
                  (Just f) -> " in file " ++ f
                  Nothing -> ""


handleNoFile :: FilePath -> Bool -> IOError -> IO ()
handleNoFile fp abort e | isDoesNotExistError e = handleNoFile'
                        | otherwise = ioError e
  where
    handleNoFile' = do
        putStrLn $ "No such file: " ++ fp
        when abort $ exitWith $ ExitFailure 102
