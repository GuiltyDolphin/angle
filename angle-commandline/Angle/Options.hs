{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module      : Angle.Options
Description : Options for Angle executable.
Copyright   : Copyright (C) 2015 Ben Moon
License     : GNU GPL, version 3
Maintainer  : GuiltyDolphin@gmail.com
Stability   : alpha

Defines the 'Options' type for use in other modules that
provide for the angle command-line software.
-}
module Angle.Options
  ( Options(..)
  , getOptions
  , OptionEnv
  , runWithOptions
  ) where

import Control.Applicative (Applicative)
import Control.Monad.Reader
import Data.List (foldl')
import Data.List.Split (splitOn)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO (hPutStrLn, stderr)

import Paths_angle (version)
import Data.Version (showVersion)


data Options = Options
  { optVerbose :: Bool
  , optFiles :: [FilePath]
  , optOutput :: Maybe FilePath
  , optInteractive :: Bool
  , optAbort :: Bool
  , optCode :: [String]
  , optNonOpts :: [String]
  , optSearchPath :: [FilePath]
  }


newtype OptionEnv a = OptionEnv
    { runOptionEnv :: ReaderT Options IO a
    } deriving ( Functor, Applicative
               , Monad, MonadIO, MonadReader Options)


runWithOptions :: Options -> OptionEnv a -> IO a
runWithOptions opts optEnv = runReaderT (runOptionEnv optEnv) opts


defaultOptions :: Options
defaultOptions = Options { optVerbose = False
                         , optFiles = []
                         , optOutput = Nothing
                         , optInteractive = False
                         , optAbort = False
                         , optCode = []
                         , optNonOpts = []
                         , optSearchPath = [ "~/.angle/lib"
                                           , "~/.angle/usr/lib"
                                           ]
                         }


options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "f" ["file"]
        (ReqArg
            (\arg opt -> return opt { optFiles = optFiles opt ++ [arg] })
            "FILE")
        "Input file"
    , Option "c" ["program"]
        (ReqArg
            (\arg opt -> return opt { optCode = optCode opt ++ [arg] })
            "TEXT")
        "Line of code to be executed directly (multiple -c's allowed)"
    , Option "" ["path"]
        (ReqArg
            (\arg opt -> return opt { optSearchPath = splitOn ":" arg })
            "PATH(S)")
        ("Paths to use instead of the defaults."
        ++ " These paths are searched when performing include")
    , Option "i" ["interactive"]
        (NoArg
            (\opt -> return opt { optInteractive = True }))
        "Run Angle in interactive (REPL) mode"
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr (usageInfo usage options)
                exitSuccess))
        "Display help"
    , Option "V" ["version"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr $ "angle " ++ showVersion version
                exitSuccess))
        "Display current program version"
    , Option "v" ["verbose"]
        (NoArg
            (\opt -> return opt { optVerbose = True }))
        "Display additional program output"
    , Option "" ["abort"]
        (NoArg
            (\opt -> return opt { optAbort = True }))
        "Abort on first file that fails to run"
    ]


-- | Program usage header.
usage :: String
usage = "Usage:  angle [OPTION...] [file]"


getOptions :: IO Options
getOptions = do
    args <- getArgs
    (opts, nonOpts) <-
        case getOpt RequireOrder options args of
            (opt,nonOpt,[]) -> return (opt,nonOpt)
            (_,_,errs) -> ioError
                (userError (concat errs ++ usageInfo usage options))
    let (fileN, nOpts) = case nonOpts of
                            (x:xs) -> ([x], xs)
                            [] -> ([], [])
    foldl' (>>=) (return defaultOptions { optFiles = fileN, optNonOpts = nOpts }) opts
    -- foldl' (>>=) (return defaultOptions { optFiles = nonOpts }) opts

