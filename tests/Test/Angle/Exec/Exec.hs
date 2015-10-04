module Test.Angle.Exec.Exec
    ( tests
    ) where

import TestHelper


--filterFun = evalParse
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
checkRes s r = monadicIO $ runEx s >>= (assert . r)


checkFail :: String -> Property
checkFail s = expectFailure $ monadicIO $ runEx s


checkResEq :: String -> LangLit -> Property
checkResEq s x = checkRes s (==x)


testLess :: Int -> Int -> Property
testLess = binTest LitBool (<) "<"


testGreater :: Int -> Int -> Property
testGreater = binTest LitBool (>) ">"


testLessEq :: Int -> Int -> Property
testLessEq = binTest LitBool (<=) "<="


testGreaterEq :: Int -> Int -> Property
testGreaterEq = binTest LitBool (>=) ">="


testAdd :: NonEmptyList Int -> Property
testAdd (NonEmpty xs) = opTest LitInt sum "+" xs


testAnd :: NonEmptyList Bool -> Property
testAnd (NonEmpty xs) = opTest LitBool and "&" xs


testSub :: NonEmptyList Int -> Property
testSub (NonEmpty xs) = opTest LitInt (foldl1 (-)) "-" xs


testMult :: NonEmptyList Int -> Property
testMult (NonEmpty xs) = opTest LitInt product "*" xs


testNot :: Bool -> Property
testNot = opTestUnary LitBool not "^"


testOr :: NonEmptyList Bool -> Property
testOr (NonEmpty xs) = opTest LitBool or "|" xs


-- | Helper function for testing binary operators.
binTest ::
  (Show a, Show b) =>
  (b -> LangLit)
  -> (a -> a -> b)
  -> String -> a -> a
  -> Property
binTest t f op x y = checkResEq st (t $ f x y)
  where st = opElts op [show x, show y]


opElts :: String -> [String] -> String
opElts op xs = concat [ "("
                       , op, " "
                       , appending " " (init xs)
                       , last xs
                       , ");"]


opStr :: (a -> LangLit) -> String -> [a] -> String
opStr t op xs = concat [ "("
                       , op, " "
                       , appending " " (init xs')
                       , last xs'
                       , ");"]
  where xs' = map (showSyn . t) xs


opTest ::
  (Show a) =>
  (a -> LangLit)
  -> ([a] -> a)
  -> String -> [a]
  -> Property
opTest t f op xs = checkResEq st (t $ f xs)
  where st = opStr t op xs


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

testBuiltinLength :: TinyList LangLit -> Property
testBuiltinLength (TinyList xs) = checkResEq toRun $ LitInt (length xs)
  where toRun = callShowSyn "length" [LitList xs]


testBuiltinLengthRange :: NonNegative Int -> NonNegative Int -> NonNegative Int -> Property
testBuiltinLengthRange (NonNegative x) (NonNegative y) (NonNegative z) =
    y > x && z > x ==> checkResEq toRun $ LitInt (length [x,z..y])
  where toRun = callShowSyn "length" [LitRange (LitInt x) (Just $ LitInt y) (Just $ LitInt z)]


testBuiltinIsNull :: LangLit -> Property
testBuiltinIsNull x = checkResEq toRun $ LitBool (x == LitNull)
  where
    toRun = callShowSyn "isNull" [x]


testBuiltinIndexBasic :: TinyList LangLit -> NonNegative Int -> Property
testBuiltinIndexBasic (TinyList xs) (NonNegative x) = x < length xs
                                        ==> checkResEq toRun $ xs !! x
  where
    toRun = callShowSyn "index" [LitInt x, LitList xs]


testBuiltinStr :: LangLit -> Property
testBuiltinStr x = checkResEq toRun $ case x of
                                        LitStr _ -> x
                                        LitChar c -> LitStr [c]
                                        r@(LitRange{}) -> LitStr $ showSyn r
                                        x' -> LitStr (showSyn x')
  where
    toRun = callShowSyn "str" [x]



testClassAdd :: LangLit -> LangLit -> Property
testClassAdd (LitInt x) (LitInt y) = checkResEq toRun $ LitInt (x + y)
  where toRun = setupAddF ++ callShow "addInts" [x, y]
testClassAdd x y = checkFail toRun
  where toRun = setupAddF ++ callShowSyn "addInts" [x, y]


testReturnSimple :: NonLambda -> Property
testReturnSimple (NonLambda x) = checkResEq toRun x
  where toRun = setupReturnSimple ++ callShowSyn "returnSimple" [x]
        setupReturnSimple = defun "returnSimple" "x" "return x;"


newtype NonLambda = NonLambda LangLit
    deriving (Show)

instance Arbitrary NonLambda where
    arbitrary = liftM NonLambda $ arbitrary `suchThat`
        (\x -> case x of
                  LitLambda{} -> False
                  _ -> True)

testReturnIfEmbedded :: Bool -> NonLambda -> NonLambda -> Property
testReturnIfEmbedded p (NonLambda x) (NonLambda y) | p = checkResEq toRun x
                           | otherwise = checkResEq toRun y
  where toRun = setupReturnIfEmbedded ++ callShowSyn "returnIfEmbedded" [LitBool p, x, y]
        setupReturnIfEmbedded = defun "returnIfEmbedded" "p, x, y" "if p then return x; else return y;"


testForLoopSimple :: TinyList LangLit -> Property
testForLoopSimple (TinyList xs) = checkResEq toRun (LitList xs)
  where toRun = for "i" (showSyn $ LitList xs) "i;"


testForLoopBreakSimple :: NonEmptyList LangLit -> Property
testForLoopBreakSimple (NonEmpty xs) = checkResEq toRun (head xs)
  where
    toRun = for "i" (showSyn $ LitList xs) (multiStmt ["i;", "break;"])


testForLoopBreakWithValue :: NonEmptyList LangLit -> LangLit -> Property
testForLoopBreakWithValue (NonEmpty xs) y = checkResEq toRun y
  where
    toRun = for "i" (showSyn $ LitList xs) (multiStmt ["i;", "break " ++ showSyn y ++ ";"])


multiStmt :: [String] -> String
multiStmt xs = concat ["{", concat xs, "}"]


for :: String -> String -> String -> String
for ident expr body = concat [ "for ", ident, " in "
                             , expr, " do ", body]


tests :: [TestTree]
tests = [ testGroup "filter tests"
          [ --testProperty "filterLess" testFilterLess
          ]
        , testGroup "basic operators"
          [ testProperty "less" testLess
          , testProperty "add" testAdd
          , testProperty "mult" testMult
          , testProperty "not" testNot
          , testProperty "sub" testSub
          , testProperty "and" testAnd
          , testProperty "or" testOr
          , testProperty "lessEq" testLessEq
          , testProperty "greaterEq" testGreaterEq
          , testProperty "greater" testGreater
          ]
        , testGroup "classes"
          [ testProperty "isAdd" testClassAdd
          ]
        , testGroup "basic functions"
          [ testProperty "returnSimple" testReturnSimple
          , testProperty "returnIfEmbedded" testReturnIfEmbedded
          ]
        , testGroup "structures"
          [ testProperty "simple for-loop" testForLoopSimple
          , testProperty "for loop break - simple" testForLoopBreakSimple
          , testProperty "for loop break - value" testForLoopBreakWithValue
          ]
        , testGroup "builtin functions"
          [ testProperty "length" testBuiltinLength
          , testProperty "length with range" testBuiltinLengthRange
          , testProperty "isNull" testBuiltinIsNull
          , testProperty "index - basic" testBuiltinIndexBasic
          , testProperty "str" testBuiltinStr
          ]
        ]








