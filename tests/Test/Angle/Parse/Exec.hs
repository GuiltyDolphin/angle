module Test.Angle.Parse.Exec
    ( tests
    ) where

import TestHelper

import Angle.Parse.Exec
import Angle.Types.Lang
import Angle.Lex.Lexer.Internal


basicAddition = ("(+ 1 2 3)", LitInt 6)


--filterFun = evalScan
--  [ "defun filter($f, xs) {"
--  , "res = [];"
--  , "for elt in xs do {"
--  , "if f(elt) then res = (+ res elt);"
--  , "}"
--  , "}" ] structDefun


setupAddF = concat [defIsInt, defAddInts]

defIsInt = defun "isInt" "x" "return (== x asType(0, x))"


defAddInts = defun "addInts" "x:@isInt, y:@isInt" "(+ x y)"


defun n a b = concat ["defun ", n, "(", a, ") ", b]

multS b = concat ["{\n", b, "\n}"]

for e es b = concat ["for ", e, " in ", es, " do ", b]

ifS c b els = concat ["if ", c, " then ", b]
              ++ maybe "" ("else "++) els

defFilterFun = defun "filter" "$f, xs"
  (multS $ concat [ "res = [];"
                  , for "elt" "xs" "if f(elt) then res = (+ res elt);"
                  ])

defLess = defun "less" "x, y" "(< x y)"

callFun f a = concat [f, "(", a, ");"]


--runEx :: String -> PropertyM IO LangLit
runEx s = let (Right r) = evalScan s program in run $ runExec $ execStmt r


checkRes :: String -> (LangLit -> Bool) -> Property
checkRes s r = monadicIO $ runEx s >>= (assertQC . r)

checkFail s = expectFailure $ monadicIO $ runEx s


checkResEq :: String -> LangLit -> Property
checkResEq s x = monadicIO $ runEx s >>= assertEqualQC x

fromLInts = map getLitInt

testLess :: Int -> Int -> Property
testLess = binTest LitBool (<) "<"


testAdd :: Int -> Int -> Property
testAdd = binTest LitInt (+) "+"


-- | Helper function for testing binary operators
binTest :: (Show b, Show c) => (a -> LangLit) -> (b -> c -> a) -> String -> b -> c -> Property
binTest t f op x y = checkResEq st (t $ f x y)
  where st = sToOp op x y


-- | Produce the neccessary padding to produce an operator string
sToOp :: (Show a, Show b) => String -> a -> b -> String
sToOp op x y = concat ["(", op, " ", show x, " ", show y, ");"]

--testFilterLess :: Int -> [Int] -> Bool
--testFilterLess x xs = res == filter (<x) xs
--  where res = fromLInts $ runE $ concat [defFilterFun, callFun "filter", k

isLeft (Left _) = True
isLeft _ = False

checkResWith s = checkRes

callShow f xs = concat [ f
                       , "("
                       , concatMap ((++", ") . show) (init xs)
                       , show (last xs)
                       , ");"
                       ]
callShowSyn f xs = concat [ f
                          , "("
                          , concatMap ((++", ") . showSyn) (init xs)
                          , showSyn (last xs)
                          , ");"
                          ]


testClassAdd :: LangLit -> LangLit -> Property
testClassAdd (LitInt x) (LitInt y) = checkResEq toRun $ LitInt (x + y)
  where toRun = setupAddF ++ callShow "addInts" [x, y]
testClassAdd x y = checkFail toRun
  where toRun = setupAddF ++ callShowSyn "addInts" [x, y]


tests = [ testGroup "filter tests"
          [ --testProperty "filterLess" testFilterLess
          ]
        , testGroup "basic operators"
          [ testProperty "less" testLess
          , testProperty "add" testAdd
          ]
        , testGroup "classes"
          [ testProperty "isAdd" testClassAdd
          ]
        ]








