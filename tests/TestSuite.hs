module Main
    ( main
    ) where

import TestHelper
import qualified Test.Angle.Lex.Lexer as Lexer
import qualified Test.Angle.Lex.Helpers as Helpers
import qualified Test.Angle.Lex.Token as Token


main :: IO ()
main = defaultMain allTests -- defaultMainWithArgs allTests ["--timeout=3", "--maximum-test-size=10"]
       
-- | Convert n seconds into microseconds
toMicroSeconds :: Integer -> Integer
toMicroSeconds n = n*10^6
allTests = localOption (Timeout (toMicroSeconds 2) "") $ testGroup "all tests" [ testGroup "lexer tests" Lexer.tests 
           , testGroup "helpers tests" Helpers.tests
           , testGroup "token tests" Token.tests
           ]
