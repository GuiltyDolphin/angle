module Main
    ( main
    ) where

import TestHelper
import qualified Test.Angle.Lex.Helpers as Helpers
import qualified Test.Angle.Lex.Lexer as Lexer
import qualified Test.Angle.Lex.Token as Token
import qualified Test.Angle.Parse.Operations as Operations
import qualified Test.Angle.Parse.Scope as Scope


main :: IO ()
main = defaultMain allTests -- defaultMainWithArgs allTests ["--timeout=3", "--maximum-test-size=10"]


-- | Convert seconds into microseconds
toMicroSeconds :: Integer -> Integer
toMicroSeconds n = n*10^(6::Integer)


allTests :: TestTree
allTests = localOption (Timeout (toMicroSeconds 3) "") $
           testGroup "all tests"
           [ testGroup "helpers tests" Helpers.tests
           , testGroup "lexer tests" Lexer.tests
           , testGroup "operations tests" Operations.tests
           , localOption (QuickCheckMaxSize 10) $
             testGroup "scope tests" Scope.tests
           , testGroup "token tests" Token.tests
           ]
