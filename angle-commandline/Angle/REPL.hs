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
import Data.List (elemIndices, isSuffixOf)
import System.Directory (canonicalizePath)
import System.IO (stdout, hFlush)

import System.Console.Haskeline

import Angle.Exec.Types
import Angle.Types.Lang
import Angle.Parse.Parser
import Angle.Exec.Exec
import Angle.Exec.Builtins

import Angle.Options


instance MonadException ExecIO where
    controlIO f = join . liftIO $ f (RunIO pure)


-- | When user is entering input over multiple lines, consolidate this
-- into a single statement.
collectLine :: String -> Int -> InputT ExecIO String
collectLine s multi =
  let newMulti = multi + incMultiBy s
  in if newMulti > 0
  then do
    nxt <- getInputLineWithInitial "> " (concat $ replicate newMulti "  ", "")
    case nxt of
      Nothing -> collectLine s multi
      Just n -> liftM (s++) $ collectLine n newMulti
  else return s
  where incMultiBy :: String -> Int
        incMultiBy xs = count '{' xs - count '}' xs


-- | Parse a line of user input and produce a text response.
runLine :: String -> ExecIO String
runLine s = do
  case evalParse s stmt of
              Left err -> pure $ show err
              Right res -> do
                toPrint <- (fmap Right $ execStmt res) `catchError` (\e -> pure (Left $ show e))
                pure $ case toPrint of
                         Left s -> s
                         Right r -> case r of
                                      (LitList xs) -> showList xs r
                                      (LitStr xs) -> showStr xs r
                                      _ -> showSyn r
    where showList xs toPrint = let fpart = LitList (take 10 xs)
                                    spart = LitList (reverse $ take 5 $ reverse xs)
                                in if length xs > 100
                                   then init (showSyn fpart) ++ ", ..., " ++ tail (showSyn spart)
                                   else showSyn toPrint
          showStr xs toPrint = if length xs > 2000
                               then let fpart = LitStr (take 1000 xs)
                                        spart = LitStr (reverse $ take 1000 $ reverse xs)
                                    in init (showSyn fpart) ++ "\n<LONG TEXT OMITTED>\n" ++ tail (showSyn spart)
                               else showSyn toPrint


count :: Eq a => a -> [a] -> Int
count x = length . elemIndices x


printSyn :: (ShowSyn a) => a -> IO ()
printSyn = putStrLn . showSyn


-- | Run interactive mode using the files as initial input.
interactiveWithFiles :: [FilePath] -> ExecIO ()
interactiveWithFiles fs = mapM_ getInteractiveFile fs >> interactiveMode

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
interactiveMode = runInputT defaultSettings loop
  where loop :: InputT ExecIO ()
        loop = do
          userInput <- getInputLine "> "
          case userInput of
            Nothing -> pure ()
            Just "exit" -> pure ()
            Just input -> runInputLine input >> loop
        runInputLine l = do
          r <- collectLine l 0
          let norm = normalizeStmt r
          s <- lift $ runLine norm
          outputStrLn s
        normalizeStmt xs = let hasSemi = isSuffixOf ";" xs
                           in if hasSemi then xs else if null xs then xs else xs ++ ";"


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
