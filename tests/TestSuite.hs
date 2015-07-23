module Main
    ( main
    ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
    
import qualified Test.Angle.Lex.Lexer as Lexer
import qualified Test.Angle.Lex.Helpers as Helpers
import qualified Test.Angle.Lex.Token as Token


main :: IO ()
main = defaultMainWithArgs allTests ["--timeout=3", "--maximum-test-size=10"]
       

allTests = [ testGroup "lexer tests" Lexer.tests 
           , testGroup "helpers tests" Helpers.tests
           , testGroup "token tests" Token.tests
           ]
