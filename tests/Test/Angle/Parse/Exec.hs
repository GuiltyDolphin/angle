module Test.Angle.Parse.Exec
    ( tests
    ) where

import Data.List (foldl')

import TestHelper

import Angle.Types.Lang
import Angle.Lex.Lexer.Internal


--filterFun = evalScan
--  [ "defun filter($f, xs) {"
--  , "res = [];"
--  , "for elt in xs do {"
--  , "if f(elt) then res = (+ res elt);"
--  , "}"
--  , "}" ] structDefun


appending :: [a] -> [[a]] -> [a]
appending x = concatMap (++x)


setupAddF :: String
setupAddF = appending "\n" [defIsInt, defAddInts]


defIsInt :: String
defIsInt = defun "isInt" "x" "return (== x asType(0, x));"

defAddInts :: String
defAddInts = defun "addInts" "x:@isInt, y:@isInt" "(+ x y);"


defun :: String -> String -> String -> String
defun n a b = concat ["defun ", n, "(", a, ") ", b]


checkRes :: String -> (LangLit -> Bool) -> Property
checkRes s r = monadicIO $ runEx s >>= (assertQC . r)


checkFail :: String -> Property
checkFail s = expectFailure $ monadicIO $ runEx s


checkResEq :: String -> LangLit -> Property
checkResEq s x = checkRes s (==x)


testLess :: Int -> Int -> Property
testLess = binTest LitBool (<) "<"


testAdd :: NonEmptyList Int -> Property
testAdd (NonEmpty xs) = opTest LitInt sum "+" xs


testMult :: NonEmptyList Int -> Property
testMult (NonEmpty xs) = opTest LitInt (foldl' (*) 1) "*" xs

testNot :: Bool -> Property
testNot = opTestUnary LitBool not "^"


-- | Helper function for testing binary operators
binTest ::
  (Show b) =>
  (a -> LangLit)
  -> (b -> b -> a)
  -> String -> b -> b
  -> Property
binTest t f op x y = checkResEq st (t $ f x y)
  where st = opStr op [x, y]


opStr :: (Show a) => String -> [a] -> String
opStr op xs = concat ["(", op, " ", appending " " (init xs'), last xs', ");"]
  where xs' = map show xs


opTest ::
  (Show b) =>
  (a -> LangLit)
  -> ([b] -> a)
  -> String -> [b]
  -> Property
opTest t f op xs = checkResEq st (t $ f xs)
  where st = opStr op xs


opTestUnary :: (a -> LangLit) -> (a -> a) -> String -> a -> Property
opTestUnary t f op x = checkResEq st (t $ f x)
  where st = concat [op, showSyn $ t x, ";"]


callShow :: (Show a) => String -> [a] -> String
callShow f xs = concat [ f, "("
                       , concatMap ((++", ") . show) (init xs)
                       , show (last xs)
                       , ");"
                       ]


callShowSyn :: (ShowSyn a) => String -> [a] -> String
callShowSyn f xs = concat [ f, "("
                          , concatMap ((++", ") . showSyn) (init xs)
                          , showSyn (last xs)
                          , ");"
                          ]


testClassAdd :: LangLit -> LangLit -> Property
testClassAdd (LitInt x) (LitInt y) = checkResEq toRun $ LitInt (x + y)
  where toRun = setupAddF ++ callShow "addInts" [x, y]
testClassAdd x y = checkFail toRun
  where toRun = setupAddF ++ callShowSyn "addInts" [x, y]


tests :: [TestTree]
tests = [ testGroup "filter tests"
          [ --testProperty "filterLess" testFilterLess
          ]
        , testGroup "basic operators"
          [ testProperty "less" testLess
          , testProperty "add" testAdd
          , testProperty "mult" testMult
          , testProperty "not" testNot
          ]
        , testGroup "classes"
          [ testProperty "isAdd" testClassAdd
          ]
        ]








