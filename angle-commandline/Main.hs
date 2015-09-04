{-|
Module      : Angle.Main
Description : Client executable for running angle.
Copyright   : Copyright (C) 2015 Ben Moon
License     : GNU GPL, version 3
Maintainer  : GuiltyDolphin@gmail.com
Stability   : alpha

Module defining main executable for running Angle programs both
interactively and non-interactively.
-}

module Main (main) where

import Control.Monad

import Angle.Options
import Angle.REPL (runInteractive)
import Angle.Interpreter (runInterpreter)


main :: IO ()
main = do
    opts <- getOptions
    let Options { optVerbose = verbose
                , optFiles = files
                , optInteractive = interactive
                } = opts
    when verbose (putStrLn "Verbose not yet implemented")
    if interactive || null files
    then runInteractive opts
    else runInterpreter opts
    return ()



