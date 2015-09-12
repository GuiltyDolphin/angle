{-|
Module      : Angle.REPL
Description : REPL for the Angle programming language.
Copyright   : Copyright (C) 2015 Ben Moon
License     : GNU GPL, version 3
Maintainer  : GuiltyDolphin@gmail.com
Stability   : alpha

Defines the REPL for use with the Angle programming language.
-}
module Angle.REPL (runInteractive) where

import Control.Monad
import Control.Monad.Except
import Data.Either (lefts, rights)
import Data.List (elemIndices)
import System.IO (stdout, hFlush)

import Angle.Exec.Types
import Angle.Types.Lang
import Angle.Lex.Lexer
import Angle.Exec.Exec
import Angle.Exec.Builtins

import Angle.Options


-- | When user is entering input over multiple lines, consolidate this
-- into a single statement.
collectLine :: String -> Int -> ExecIO String
collectLine s multi =
  if multi + incMultiBy s > 0
  then do
    nxt <- liftIO getLine
    liftM (s++) $ collectLine nxt (multi + incMultiBy s)
  else return s


-- | Parse and execute a line of user input.
runLine :: String -> ExecIO ()
runLine s = do
  r <- collectLine s 0
  case evalScan r stmt of
              Left err -> liftIO $ print err
              Right res -> do
                toPrint <- execStmt res `catchError` (\e -> liftIO (print e) >> throwError e)
                liftIO $ printSyn toPrint


incMultiBy :: String -> Int
incMultiBy xs = count '{' xs - count '}' xs


count :: Eq a => a -> [a] -> Int
count x = length . elemIndices x


printSyn :: (ShowSyn a) => a -> IO ()
printSyn = putStrLn . showSyn


-- | Run interactive mode using the files as initial input.
interactiveWithFiles :: [FilePath] -> ExecIO ()
interactiveWithFiles fs = do
    fileSources <- liftIO $ mapM readFile fs
    let asStmts = map (`evalScan` program) fileSources
    if not . null $ lefts asStmts
    then liftIO $ mapM_ (putStrLn . ("failed to load file: " ++))
        [x ++ "\n" ++ show r | (x,Left r) <- zip fs asStmts]
    else do
        let overStmt = MultiStmt (rights asStmts)
        -- execStmt overStmt
        toPrint <- execStmt overStmt
            `catchError` (\e -> liftIO (print e) >> throwError e)
        liftIO $ printSyn toPrint
        -- runExecIOEnv initialEnvNotMain (execStmt overStmt)
        interactiveMode
        return ()
    return ()
-- withSource s =
--   case evalScan ('{':s++"}") stmt of
--     Left err -> liftIO (print err) >> interactiveMode
--     Right res -> do
--                  toPrint <- execStmt res `catchError` (\e -> liftIO (print e) >> throwError e)
--                  liftIO $ printSyn toPrint
--                  interactiveMode


-- | Runs a REPL using the Angle programming language.
interactiveMode :: ExecIO ()
interactiveMode = do
  userInput <- liftIO $ prompt "> "
  unless (userInput=="exit")
           (do
             runLine userInput `catchError` const interactiveMode
             interactiveMode)


prompt :: String -> IO String
prompt s = do
    putStr s
    hFlush stdout
    getLine


-- | Run Angle in interactive mode.
runInteractive :: Options -> IO ()
runInteractive opts = do
  let Options { optFiles = files
              } = opts
  when (length files > 1)
      (putStrLn $ "Warning: Running interactive with multiple files, "
          ++ "this may go badly!")
  if not $ null files
  then runExecIOEnv initialEnvNotMain (interactiveWithFiles files)
  else runExecIOEnv initialEnvNotMain interactiveMode
  return ()
