{-# LANGUAGE DeriveDataTypeable #-}
module Angle.Exec.REPL
    ( main
    ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Data.List (elemIndices)

import System.Console.CmdArgs hiding (program)
import qualified System.Console.CmdArgs as CA

import Angle.Parse.Parser
import Angle.Exec.Builtins
import Angle.Exec.Exec
import Angle.Exec.Types
import Angle.Types.Lang

data ReplOptions = ReplOptions
    { file :: String
    } deriving (Show, Data, Typeable)


replOptions :: ReplOptions
replOptions = ReplOptions
              { file = def &= typFile
              }
              &= CA.program "angle-repl"
              &= summary "repl for angle programming language"


--processLine :: String -> IO ()
--processLine s = do
--  case evalScan s program of
--    Left err -> print err
--    Right res -> runExecIOEnv startEnv (execStmt res) >>= print

-- execLine :: Env -> String -> IO Env
-- execLine e s = case evalScan s program of
--                  Left err -> print err
--                  Right res -> runExecIOEnv (execStmt e) res >>= print

collectLine :: String -> Int -> ExecIO String
collectLine s multi =
  if multi + incMultiBy s > 0
  then do
    nxt <- liftIO getLine
    liftM (s++) $ collectLine nxt (multi + incMultiBy s)
  else return s
  -- then return s
  -- else do
  --    nxt <- liftIO getLine
  --    if shouldDecMulti s
  --    then liftM (s++) $ collectLine nxt (multi-1)
  --    else if shouldIncMulti s
  --         then liftM (s++) $ collectLine nxt (multi+1)
  --         else liftM (s++) $ collectLine nxt multi

runLine :: String -> ExecIO ()
runLine s = do
  r <- collectLine s 0
  case evalScan r stmt of
              Left err -> liftIO $ print err
              Right res -> do
                toPrint <- execStmt res `catchError` (\e -> liftIO (print e) >> throwError e)
                liftIO $ printSyn toPrint


--shouldIncMulti :: String -> Bool
--shouldIncMulti xs = count '{' xs > count '}' xs

incMultiBy :: String -> Int
incMultiBy xs = count '{' xs - count '}' xs



--shouldDecMulti :: String -> Bool
--shouldDecMulti xs = count '}' xs > count '{' xs


count :: Eq a => a -> [a] -> Int
count x = length . elemIndices x


printSyn :: (ShowSyn a) => a -> IO ()
printSyn = putStrLn . showSyn


withSource :: String -> ExecIO ()
withSource s =
  case evalScan ('{':s++"}") stmt of
    Left err -> liftIO (print err) >> mainProg
    Right res -> do
                 toPrint <- execStmt res `catchError` (\e -> liftIO (print e) >> throwError e)
                 liftIO $ printSyn toPrint
                 mainProg



mainProg :: ExecIO ()
mainProg = do
  liftIO $ putStr "> "
  userInput <- liftIO getLine
  unless (userInput=="exit")
           (do
             runLine userInput `catchError` const mainProg
             mainProg)


--runMain :: ReplOptions -> IO ()
--runMain opts = undefined


runWithSource :: String -> IO ()
runWithSource s = do
  runExecIOEnv startEnv $ withSource s
  return ()

main :: IO ()
main = do
  margs <- cmdArgs replOptions
  unless (null $ file margs)
       (readFile (file margs) >>= runWithSource)
  runExecIOEnv startEnv mainProg
  return ()
  -- runExecIOBasic mainProg
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
