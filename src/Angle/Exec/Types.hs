{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : Angle.Exec.Types
Description : Exports functions from Angle.Exec.Types.Internal
Copyright   : Copyright (C) 2015 Ben Moon
License     : GNU GPL, version 3
Maintainer  : GuiltyDolphin@gmail.com
Stability   : alpha

-}
module Angle.Exec.Types
    ( ExecIO
    , runExecIOEnv
    , iterToLit
    , fromIter
    , returnVal
    , fromEnumL
    , isInfiniteRange
    -- ** Execution Environment
    , Env(..)
    , getEnv
    , updateEnv
    , basicEnv
    , getEnvValue
    , setEnvSynRep
    , updatePos

    , popEnvCall
    , pushEnvCall

    , updateStmt
    ) where

import Angle.Exec.Types.Internal
