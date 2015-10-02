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
