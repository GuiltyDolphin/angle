module Test.Angle.Parse.Parser
    ( tests
    ) where

import Angle.Parse.Parser.Internal
import qualified Test.QuickCheck.Property as P
import Angle.Types.Lang
import TestHelper


testShowSynStmt :: Stmt -> Bool
testShowSynStmt x = showSynTest x stmt


testShowSynSingStmt :: SingStmt -> Bool
testShowSynSingStmt x = showSynTest x singStmt


testShowSynLangStruct :: LangStruct -> P.Result
testShowSynLangStruct x = prettySyn x langStruct -- showSynTest x langStruct


testShowSynExpr :: Expr -> P.Result
testShowSynExpr x = prettySyn x expr -- showSynTest x expr


testShowSynLambda :: Lambda -> P.Result
testShowSynLambda x = prettySyn x lambda -- showSynTest x lambda


showSynTest :: (ShowSyn a, Eq a) => a -> Parser a -> Bool
showSynTest x sc = evalParse (showSyn x) sc == Right x


prettySyn :: (ShowSyn a, Eq a) => a -> Parser a -> P.Result
prettySyn = withPretty f p
    where p x sc = either (const $ "Could not parse: \n" ++ showSyn x)
                     showSyn (evalParse (showSyn x) sc)
          f = showSynTest


withPretty :: (a -> Parser b -> Bool) -- ^ Test function to apply
           -> (a -> Parser b -> String) -- ^ Prettify function
           -> a -> Parser b -> P.Result
withPretty f pretty x sc = P.result
                             { P.ok = Just b
                             , P.reason = if b
                                          then ""
                                          else pretty x sc
                             }
    where b = f x sc


escapedStr :: String -> String
escapedStr xs = "\"" ++ xs ++ "\""



newtype NoQuoteString = NoQuoteString String
  deriving (Show, Eq)

instance Arbitrary NoQuoteString where
  arbitrary = liftM NoQuoteString $ suchThat arbitrary ("\"" `noneElem`)


testLitStrEmpty :: Bool
testLitStrEmpty = evalParse "\"\"" litStr == Right (LitStr "")


testLitStr :: String -> Property
testLitStr x = "\"\\" `noneElem` x ==> litStrShow x litStr
-- testLitStr x = '"' `notElem` x ==> evalParse (escapedStr x) litStr == Right (LitStr x)
--


noneElem :: (Eq a) => [a] -> [a] -> Bool
noneElem xs ys = all (`notElem` ys) xs


litStrShow :: String -> Parser LangLit -> P.Result
litStrShow = withPretty f p
    where f x sc = evalParse (escapedStr x) sc == Right (LitStr x)
          p x sc = either (const "got an error!\n") showSyn (evalParse (escapedStr x) sc)


testLitInt :: Int -> Bool
testLitInt x = evalParse (show x) litInt == Right (LitInt x)


testLitBool :: Bool
testLitBool = evalParse "true" litBool == Right (LitBool True)
              && evalParse "false" litBool == Right (LitBool False)


-- testRange :: LangLit -> LangLit -> LangLit -> Bool
-- testRange x y z = evalParse toTest litRange == Right expected
--     where toTest = concat ["(", showSyn x, "..", showSyn y, "..", showSyn z, ")"]
--           expected = LitRange x y (Just z)


testEmptyCall :: Bool
testEmptyCall = evalParse "foo()" exprFunCall
 == Right (ExprFunCall (LangIdent "foo") False [])


testOpAdd :: Bool
testOpAdd = evalParse "(+ 1 2)" langOp
 == Right (MultiOp OpAdd [ExprLit (LitInt 1), ExprLit (LitInt 2)])


testOpNeg :: Bool
testOpNeg = evalParse "-x" langOp ==  Right (SpecOp OpNeg (ExprIdent (LangIdent "x")))


tests :: [TestTree]
tests = [ testGroup "literals"
          [ testProperty "boolean" testLitBool
--          , testProperty "range" testRange
          , testProperty "integer" testLitInt
          , testProperty "string" testLitStr
          , testProperty "empty string" testLitStrEmpty
--          , testProperty "float" testLitFloat
          ]
        , testGroup "functions"
          [ testProperty "no args from empty call" $ once testEmptyCall
          ]
        , testGroup "operators"
          [ testProperty "addition operator" $ once testOpAdd
          , testProperty "negation operator" $ once testOpNeg
          ]
        , localOption (QuickCheckMaxSize 10) $
          testGroup "show syntax"
          [ localOption (QuickCheckMaxSize 9) $
            testProperty "Stmt" testShowSynStmt
          , testProperty "SingStmt" testShowSynSingStmt
          , localOption (QuickCheckMaxSize 9) $
            testProperty "LangStruct" testShowSynLangStruct
          , testProperty "Expr" testShowSynExpr
          , localOption (QuickCheckMaxSize 10) $ testProperty "Lambda" testShowSynLambda
          ]
        ]
