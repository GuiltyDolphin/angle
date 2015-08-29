{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module TestHelper
    ( module Test.Tasty
    , module Test.Tasty.QuickCheck
    , module Test.Tasty.HUnit
    , Scanner
    , evalScan
    , monadicEither
--    , monadicExec
    , monadicIO
    , assertQC
    , assertEqualQC
    , run
    , maxSized
    , runExecIOBasic
    , runExec
    , runEx
    ) where


import Control.Applicative ((<*>), (<$>))
import Control.Monad.State
import Data.Char (isAlpha, isAlphaNum)
import Data.List (zipWith4)
import qualified Data.Map as M

import Test.QuickCheck
import Test.QuickCheck.Monadic hiding (assert)
import qualified Test.QuickCheck.Monadic as Monadic
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Angle.Lex.Helpers (evalScan, Scanner)
import Angle.Parse.Scope
import Angle.Parse.Error
import Angle.Parse.Types.Internal
import Angle.Scanner (SourcePos(..))
import Angle.Types.Lang
import Angle.Lex.Lexer (program)
import Angle.Parse.Exec (execStmt)


instance Arbitrary LangLit where
    arbitrary = frequency
                [ (7, liftArby (LitStr . getValidLitStr))
                , (9, liftArby LitInt)
                , (9, liftArby LitFloat)
                , (1, liftM LitList (liftArby getSmallList))
                , (9, liftArby LitBool)
                --, (1, liftArby3 LitRange)
                , (9, return LitNull)
                ]
    shrink (LitList xs) = shrink1 LitList xs
    shrink (LitInt x) = shrink1 LitInt x
    shrink (LitStr x) = shrink1 LitStr x
    shrink (LitFloat x) = shrink1 LitFloat x
    shrink (LitBool x) = shrink1 LitBool x
    shrink (LitRange x y z) = shrink3 LitRange x y z
    shrink (LitLambda x) = shrink1 LitLambda x
    shrink (LitChar x) = shrink1 LitChar x
    shrink LitNull = [LitNull]
    shrink _ = undefined


instance Arbitrary SingStmt where
    arbitrary = frequency
                [ (7, liftArby2 StmtAssign)
                , (2, liftArby StmtStruct)
                , (5, liftArby StmtExpr)
                , (8, liftArby StmtReturn)
                ]
    shrink (StmtAssign x y) =
        shrink2 StmtAssign x y
    shrink (StmtStruct x) = shrink1 StmtStruct x
    shrink (StmtExpr x) = shrink1 StmtExpr x
    shrink (StmtReturn x) = shrink1 StmtReturn x
    shrink _ = undefined


instance Arbitrary LangStruct where
    arbitrary = frequency
                [ (3, liftArby3 StructFor)
                , (3, liftArby2 StructWhile)
                , (3, liftM3 StructIf arbitrary (liftArby MultiStmt) arbitrary)
                , (1, liftArby2 StructDefun)
                -- , liftArby StructReturn -- Not using atm
                ]
    shrink (StructFor x y z) = shrink3 StructFor x y z
    shrink (StructWhile x y) = shrink2 StructWhile x y
    shrink (StructIf x y z) = shrink3 StructIf x y z
    shrink (StructDefun x y) = shrink2 StructDefun x y
    shrink _ = undefined


instance Arbitrary Lambda where
    arbitrary = do
      args <- arbitrary
      body <- arbitrary
      return Lambda { lambdaArgs=args, lambdaBody=body}
    shrink (Lambda x y) = shrink2 Lambda x y


instance Arbitrary Expr where
    arbitrary = frequency
                [ (15, liftArby ExprIdent)
                , (9, liftArby  ExprLit)
                , (1, liftM2 ExprFunCall arbitrary (liftArby getTinyList))
                , (4, liftArby  ExprOp)
                --, (4, liftArby ExprLambda)
                , (4, liftArby ExprFunIdent)
                ]
    shrink (ExprIdent x) = shrink1 ExprIdent x
    shrink (ExprLit x) = shrink1 ExprLit x
    shrink (ExprFunCall x y) = shrink2 ExprFunCall x y
    shrink (ExprOp x) = shrink1 ExprOp x
    shrink (ExprLambda x) = shrink1 ExprLambda x
    shrink (ExprFunIdent x) = shrink1 ExprFunIdent x
    shrink (ExprList x) = map ExprList $ shrink x
    shrink _ = undefined


instance Arbitrary ArgSig where
    arbitrary = do
      args <- liftArby getTinyList
      catchArg <- arbitrary
      return ArgSig { Angle.Types.Lang.stdArgs = args, catchAllArg = catchArg }
    shrink (ArgSig x y) = shrink2 ArgSig x y


instance Arbitrary ArgElt where
    arbitrary = liftArby3 ArgElt
    shrink (ArgElt x y z) = shrink3 ArgElt x y z


shrink1 :: Arbitrary a => (a -> b) -> a -> [b]
shrink1 f x = map f (shrink x)


shrink2 :: (Arbitrary a, Arbitrary b) => (a -> b -> c) -> a -> b -> [c]
shrink2 f x y = zipWith f (shrink x) (shrink y)


shrink3 :: (Arbitrary c, Arbitrary b, Arbitrary a) =>
     (a -> b -> c -> d) -> a -> b -> c -> [d]
shrink3 f x y z = zipWith3 f (shrink x) (shrink y) (shrink z)


shrink4
  :: (Arbitrary d, Arbitrary c, Arbitrary b, Arbitrary a) =>
     (a -> b -> c -> d -> e) -> a -> b -> c -> d -> [e]
shrink4 f w x y z = zipWith4 f (shrink w) (shrink x) (shrink y) (shrink z)


instance Arbitrary ClassRef where
    arbitrary = liftArby ClassRef
    shrink (ClassRef x) = shrink1 ClassRef x


instance Arbitrary AnnType where
    arbitrary = elements [AnnFun, AnnLit]
    shrink AnnFun = [AnnLit]
    shrink AnnLit = [AnnFun]
    shrink _ = undefined


instance Arbitrary Stmt where
    arbitrary = frequency
                [ (9, liftArby2 SingleStmt)
                , (1, liftM MultiStmt (liftArby getTinyList))
                ]
    shrink (SingleStmt x p) = shrink2 SingleStmt x p
    shrink (MultiStmt xs) = shrink1 MultiStmt xs


instance Arbitrary LangOp where
    arbitrary = frequency
                [ (7, liftM SpecOp (liftArby getSpecOp) >>= checkOp)
                , (3, liftM MultiOp (liftArby getMultiOp) >>= liftArby)
                ]
        where checkOp f = do -- Prevent -ve on numbers
                x <- arbitrary
                case f x of
                  (SpecOp OpNeg (ExprLit (LitInt _))) -> arbitrary
                  (SpecOp OpNeg (ExprLit (LitFloat _))) -> arbitrary
                  r -> return r

    shrink (SpecOp x y) = shrink2 SpecOp x y
    shrink (MultiOp x ys) = shrink2 MultiOp x ys


instance Arbitrary Op where
    arbitrary = oneof [ liftArby getMultiOp
                      , liftArby getSpecOp]



liftArby :: (Arbitrary a) => (a -> b) -> Gen b
liftArby f = liftM f arbitrary


liftArby2 :: (Arbitrary a, Arbitrary b) => (a -> b -> c) -> Gen c
liftArby2 f = liftM2 f arbitrary arbitrary


liftArby3 :: (Arbitrary a, Arbitrary b, Arbitrary c) => (a -> b -> c -> d) -> Gen d
liftArby3 f = liftM3 f arbitrary arbitrary arbitrary


liftArby4 :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => (a -> b -> c -> d -> e) -> Gen e
liftArby4 f = liftM4 f arbitrary arbitrary arbitrary arbitrary


newtype ValidLitStr =
    ValidLitStr { getValidLitStr :: String }


instance Arbitrary ValidLitStr where
    arbitrary = liftArby (ValidLitStr . filter (/='"'))


instance Arbitrary LangIdent where
    arbitrary = liftM LangIdent $ validIdent `suchThat` isValidIdent
    shrink (LangIdent x) = map LangIdent (filter isValidIdent (shrink x))


validIdent :: Gen String
validIdent = (:) <$> chooseAlpha <*> listOf chooseAlphaNum
    where chooseAlpha = oneof [choose ('a','z'), choose ('A','Z')]
          chooseDigit = choose ('0','9')
          chooseAlphaNum = oneof [chooseAlpha, chooseDigit]


newtype SpecOp = ArbySpecOp { getSpecOp :: Op }


newtype MultiOp = ArbyMultiOp { getMultiOp :: Op }


instance Arbitrary SpecOp where
    arbitrary = elements $ map ArbySpecOp [OpNeg, OpNot]


instance Arbitrary MultiOp where
    arbitrary = elements $ map ArbyMultiOp
                [ OpAdd
                , OpAnd
                , OpConcat
                , OpDiv
                , OpEq
                , OpGreater
                , OpGreaterEq
                , OpLess
                , OpLessEq
                , OpMult
                , OpOr
                , OpSub
                ]


newtype ValidComment = ValidComment String


instance Arbitrary ValidComment where
    arbitrary = liftM ValidComment $ arbitrary `suchThat` isValidComment
            where isValidComment x | '\n' `elem` x = False
                                   | otherwise = True


maxSmallListLength :: Int
maxSmallListLength = 50


maxTinyListLength :: Int
maxTinyListLength = 10


newtype SmallList a = SmallList { getSmallList :: [a] }
    deriving (Show)


newtype TinyList a = TinyList { getTinyList :: [a] }
    deriving (Show)


instance (Arbitrary a) => Arbitrary (SmallList a) where
    arbitrary = sized $ \s -> do
                  n <- choose (0,s`min`maxSmallListLength)
                  xs <- vector n
                  return (SmallList xs)
    shrink (SmallList xs) = shrink1 SmallList xs


instance (Arbitrary a) => Arbitrary (TinyList a) where
    arbitrary = sized $ \s -> do
                  n <- choose (0,s`min`maxTinyListLength)
                  xs <- vector n
                  return (TinyList xs)
    shrink (TinyList xs) = shrink1 TinyList xs


instance (Arbitrary a) => Arbitrary (BindEnv a) where
    arbitrary = liftArby bindEnvFromList
    shrink (BindEnv x) = shrink1 bindEnvFromList (M.toList x)


instance (Arbitrary a) => Arbitrary (VarVal a) where
    arbitrary = liftArby2 VarVal
    shrink (VarVal x y) = shrink2 VarVal x y


instance Arbitrary Scope where
    arbitrary = liftArby3 Scope
    shrink (Scope w x y) = shrink3 Scope w x y


-- Extracts a property from monadic Either code, giving
-- a failing property if the result is a Left.
monadicEither :: PropertyM (Either e) a -> Property
monadicEither = monadic (\e -> case e of
                                 Left _ -> property False
                                 Right r -> r)


runExec :: ExecIO a -> IO a
runExec e = do
  x <- runExecIOBasic e
  case x of
    Left _ -> fail "runExec failed"
    Right r -> return r


runEx :: String -> PropertyM IO LangLit
runEx s = let (Right r) = evalScan s program in run $ runExec $ execStmt r

-- monadicExec :: PropertyM ExecIO a -> PropertyM ExecIO (IO (Either AngleError a))
-- monadicExec e = do
--  x <- e
--  return (runExecIOBasic x)





instance Arbitrary LangType where
    arbitrary = elements langTypes
    shrink x = filter (/=x) langTypes


langTypes :: [LangType]
langTypes = [LTStr, LTInt, LTFloat, LTList
            , LTBool, LTRange, LTNull ]


instance Arbitrary SourceRef where
    arbitrary = do
      start <- arbitrary
      end <- arbitrary
      return $ SourceRef (start, end)
    shrink (SourceRef x) = shrink1 SourceRef x


instance Arbitrary SourcePos where
    arbitrary = do
      f <- liftArby getPositive
      s <- liftArby getPositive
      t <- liftArby getPositive
      return $ SourcePos (f, s, t)
    shrink (SourcePos x) = shrink1 SourcePos x


assertEqualQC :: (Monad m, Eq a) => a -> a -> PropertyM m ()
assertEqualQC x = assertQC . (==x)


maxSized :: (Testable prop) => Int -> prop -> Property
maxSized x = mapSize (min x)


assertQC :: (Monad m) => Bool -> PropertyM m ()
assertQC = Monadic.assert

isValidIdent :: String -> Bool
isValidIdent [] = False
isValidIdent (x:xs) = isAlpha x && all isAlphaNum xs
