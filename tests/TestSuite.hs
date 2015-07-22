module Main
    ( main
    ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
    
import qualified Test.Angle.Lex.Lexer as Lexer
import qualified Test.Angle.Lex.Helpers as Helpers
    


main :: IO ()
main = defaultMain allTests
       

allTests = [ testGroup "lexer tests" Lexer.tests 
           , testGroup "helpers tests" Helpers.tests
           ]
