module Main
    ( main
    ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
    
import qualified Test.Angle.Lex.Lexer as Lexer
    


main :: IO ()
main = defaultMain allTests
       

allTests = [ testGroup "lexer tests" Lexer.tests ]
