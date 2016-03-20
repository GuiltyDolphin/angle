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
import Control.Monad.State (put)
import Data.Either (lefts, rights)
import Data.List (elemIndices)
import System.Directory (canonicalizePath)
import System.IO (stdout, hFlush)

import Angle.Exec.Types
import Angle.Types.Lang
import Angle.Parse.Parser
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
  case evalParse r stmt of
              Left err -> liftIO $ print err
              Right res -> do
                envBefore <- getEnv
                toPrint <- execStmt res `catchError` (\e -> liftIO (print e) >> updateEnv envBefore >> throwError e)
                let toShow = case toPrint of
                                (LitList xs) -> let fpart = LitList (take 10 xs)
                                                    spart = LitList (reverse $ take 5 $ reverse xs)
                                                in if length xs > 100
                                                  then init (showSyn fpart) ++ ", ..., " ++ tail (showSyn spart)
                                                  else showSyn toPrint
                                (LitStr xs) -> if length xs > 2000
                                              then let fpart = LitStr (take 1000 xs)
                                                       spart = LitStr (reverse $ take 1000 $ reverse xs)
                                                   in init (showSyn fpart) ++ "\n<LONG TEXT OMITTED>\n" ++ tail (showSyn spart)
                                              else showSyn toPrint
                                _ -> showSyn toPrint
                liftIO $ putStrLn toShow


incMultiBy :: String -> Int
incMultiBy xs = count '{' xs - count '}' xs


count :: Eq a => a -> [a] -> Int
count x = length . elemIndices x


printSyn :: (ShowSyn a) => a -> IO ()
printSyn = putStrLn . showSyn


-- | Run interactive mode using the files as initial input.
interactiveWithFiles :: [FilePath] -> ExecIO ()
interactiveWithFiles fs = do
    mapM_ getInteractiveFile fs
    interactiveMode
    return ()

-- interactiveWithFiles fs = do
--     fileSources <- liftIO $ mapM readFile fs
--     let asStmts = map (`evalParse` program) fileSources
--     if not . null $ lefts asStmts
--     then liftIO $ mapM_ (putStrLn . ("failed to load file: " ++))
--         [x ++ "\n" ++ show r | (x,Left r) <- zip fs asStmts]
--     else do
--         let overStmt = MultiStmt (rights asStmts)
--         -- execStmt overStmt
--         toPrint <- execStmt overStmt
--             `catchError` (\e -> liftIO (print e) >> throwError e)
--         liftIO $ printSyn toPrint
--         -- runExecIOEnv initialEnvNotMain (execStmt overStmt)
--         interactiveMode
--         return ()
--     return ()

getInteractiveFile :: FilePath -> ExecIO ()
getInteractiveFile fp = do
    canPath <- liftIO $ canonicalizePath fp
    source <- liftIO $ readFile fp
    currEnv <- getEnv
    put currEnv { currentFile = Just canPath }
    let asStmt = evalParse source program
    case asStmt of
        Left err -> liftIO $ print err
        Right s -> do
            toPrint <- execStmt s `catchError` (\e -> liftIO (print e) >> throwError e)
            liftIO $ printSyn toPrint
-- withSource s =
--   case evalParse ('{':s++"}") stmt of
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
