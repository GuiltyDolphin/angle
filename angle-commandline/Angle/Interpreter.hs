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
import Control.Monad.Reader
import System.Directory (canonicalizePath)
import System.IO.Error
import System.Exit

import Text.Parsec.Error

import Angle.Options
import Angle.Exec.Builtins
import Angle.Exec.Error
import Angle.Exec.Exec
import Angle.Exec.Types
import Angle.Parse.Parser
import Angle.Types.Lang (Stmt)


runInterpreter :: Options -> IO ()
runInterpreter opts = do
    let Options { optFiles = files
                , optCode = code
                } = opts

    runWithOptions opts $ runFiles files
    runWithOptions opts $ runCodes code



runFiles :: [FilePath] -> OptionEnv ()
runFiles = mapM_ runFile


runFile :: FilePath -> OptionEnv ()
runFile fp = do
    source' <- liftIO $ tryIOError $ readFile fp
    case source' of
        Left e -> handleNoFile fp e
        Right source -> runLex (Just fp) source
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


runLex :: Maybe FilePath -> String -> OptionEnv ()
runLex fp source =
    case evalParse source program of
        Left err -> handleSyntax fp err
        Right toExec -> executeProg fp source toExec


executeProg :: Maybe FilePath -> String -> Stmt -> OptionEnv ()
executeProg fp source toExec = do
    paths <- liftM optSearchPath ask
    currPath <- case fp of
                 Nothing -> return Nothing
                 Just pth -> liftM Just $ liftIO $ canonicalizePath pth
    let execEnv = initialEnvMain { sourceText = source
                                 , angleLibPath = paths
                                 , currentFile = currPath
                                 }
    res <- liftIO $ runExecIOEnv execEnv (execStmt toExec)
    case res of
        Left err -> handleRuntime fp err
        Right _ -> return ()


runCodes :: [String] -> OptionEnv ()
runCodes = mapM_ runCode


runCode :: String -> OptionEnv ()
runCode = runLex Nothing


handleSyntax :: Maybe FilePath -> ParseError -> OptionEnv ()
handleSyntax = handleErr "Syntax error" 100


handleRuntime :: Maybe FilePath -> AngleError -> OptionEnv ()
handleRuntime = handleErr "Runtime error" 101



fileError :: Maybe FilePath -> String
fileError fp = case fp of
                (Just f) -> " in file " ++ f
                Nothing -> ""


handleErr :: (Show e) => String -> Int -> Maybe FilePath -> e -> OptionEnv ()
handleErr errMsgStart errCode fp err = do
    abort <- liftM optAbort ask
    liftIO $ putStrLn (errMsgStart ++ fileError fp) >> print err
    liftIO $ when abort $ exitWith $ ExitFailure errCode


handleNoFile :: FilePath -> IOError -> OptionEnv ()
handleNoFile fp e | isDoesNotExistError e = handleNoFile'
                  | otherwise = liftIO $ ioError e
  where
    handleNoFile' = do
        abort <- liftM optAbort ask
        liftIO $ putStrLn $ "No such file: " ++ fp
        liftIO $ when abort $ exitWith $ ExitFailure 102
