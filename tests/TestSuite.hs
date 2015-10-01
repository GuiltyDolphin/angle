module Main
    ( main
    ) where

import TestHelper
import qualified Test.Angle.Parse.Helpers as Helpers
import qualified Test.Angle.Parse.Parser as Parser
import qualified Test.Angle.Parse.Token as Token
import qualified Test.Angle.Exec.Operations as Operations
import qualified Test.Angle.Exec.Scope as Scope
import qualified Test.Angle.Exec.Exec as Exec
import qualified Test.Angle.Exec.Types as ExecTypes


main :: IO ()
main = defaultMain allTests -- defaultMainWithArgs allTests ["--timeout=3", "--maximum-test-size=10"]


-- | Convert seconds into microseconds
toMicroSeconds :: Integer -> Integer
toMicroSeconds n = n*10^(6::Integer)


allTests :: TestTree
allTests = localOption (Timeout (toMicroSeconds 3) "") $
           testGroup "all tests"
           [ testGroup "helpers tests" Helpers.tests
           , localOption (QuickCheckMaxSize 10) $
             testGroup "lexer tests" Parser.tests
           , testGroup "operations tests" Operations.tests
           , localOption (QuickCheckMaxSize 10) $
             testGroup "scope tests" Scope.tests
           , testGroup "token tests" Token.tests
           , localOption (QuickCheckMaxSize 10) $
             testGroup "exec tests" Exec.tests
           , testGroup "exec-types tests" ExecTypes.tests
           ]
