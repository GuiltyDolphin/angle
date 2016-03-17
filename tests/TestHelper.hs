{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module TestHelper
    ( module Test.Tasty
    , module Test.Tasty.QuickCheck
    , module Angle.Types.Lang
    , assert
    , Parser
    , evalParse
    , monadicEither
    , liftM
    , monadicIO
    , assertEqual
    , run
    , maxSized
    , runExecIOBasic
    , runExec
    , runEx
    , SmallList(..)
    , TinyList(..)
    ) where


import Control.Applicative ((<*>), (<$>))
import Control.Monad.State
import Data.Char (isAlpha, isAlphaNum)
import qualified Data.Map as M

import Text.Parsec.Pos

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck

import Angle.Parse.Helpers (evalParse, Parser)
import Angle.Parse.Token (keywords)
import Angle.Exec.Types.Internal
import Angle.Types.Lang
import Angle.Types.Scope
import Angle.Parse.Parser (program)
import Angle.Exec.Exec (execStmt)


instance Arbitrary LangLit where
    arbitrary = frequency
                [ (7, liftArby (LitStr . getValidLitStr))
                , (9, liftArby LitInt)
                , (9, liftArby LitFloat)
                , (1, liftM LitList (liftArby getTinyList))
                , (9, liftArby LitBool)
                , (1, arbyRange)
                , (6, liftArby LitChar)
                , (1, liftArby LitLambda)
                --, (9, return LitNull)
                ]
      where arbyRange = do
              x <- suchThat arbitrary enumType
              [y,z] <- suchThat (vector 2) (allType (typeOf x))
              (b1, b2) <- arbitrary
              let y' = if b1 then Just y else Nothing
                  z' = if b2 then Just z else Nothing
              return $ LitRange x y' z'


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
                ]
    shrink (StructFor x y z) = shrink3 StructFor x y z
    shrink (StructWhile x y) = shrink2 StructWhile x y
    shrink (StructIf x y z) = shrink3 StructIf x y z
    shrink (StructDefun x y) = shrink2 StructDefun x y
    shrink (StructTryCatch x y) = shrink2 StructTryCatch x y


instance Arbitrary Lambda where
    arbitrary = do
      args <- arbitrary
      body <- arbitrary
      return Lambda { lambdaArgs=args, lambdaBody=body, lambdaScope = Nothing}
    shrink (Lambda x y z) = shrink3 Lambda x y z


instance Arbitrary Expr where
    arbitrary = frequency
                [ (15, liftArby ExprIdent)
                , (9, liftArby  ExprLit)
                , (1, liftM3 ExprFunCall arbitrary arbitrary (liftArby getTinyList))
                , (4, liftArby  ExprOp)
                , (4, liftArby ExprFunIdent)
                ]


instance Arbitrary ArgSig where
    arbitrary = do
      args <- liftArby getTinyList
      catchArg <- arbitrary
      return ArgSig { Angle.Types.Lang.stdArgs = args, catchAllArg = catchArg }
    shrink (ArgSig x y) = shrink2 ArgSig x y


instance Arbitrary ArgElt where
    arbitrary = liftArby3 ArgElt
    shrink (ArgElt x y z) = shrink3 ArgElt x y z


instance Arbitrary CatchArg where
    arbitrary = liftArby2 CatchArg


shrink1 :: Arbitrary a => (a -> b) -> a -> [b]
shrink1 f x = map f (shrink x)


shrink2 :: (Arbitrary a, Arbitrary b) => (a -> b -> c) -> a -> b -> [c]
shrink2 f x y = zipWith f (shrink x) (shrink y)


shrink3 :: (Arbitrary c, Arbitrary b, Arbitrary a) =>
     (a -> b -> c -> d) -> a -> b -> c -> [d]
shrink3 f x y z = zipWith3 f (shrink x) (shrink y) (shrink z)


instance Arbitrary ConstrRef where
    arbitrary = liftArby2 ConstrRef
    shrink (ConstrRef x y) = shrink2 ConstrRef x y


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
    arbitrary = liftM MultiOp (liftArby getMultiOp) >>= liftArby
    shrink (MultiOp x ys) = shrink2 MultiOp x ys


instance Arbitrary Op where
    arbitrary = liftArby getMultiOp



liftArby :: (Arbitrary a) => (a -> b) -> Gen b
liftArby f = liftM f arbitrary


liftArby2 :: (Arbitrary a, Arbitrary b) => (a -> b -> c) -> Gen c
liftArby2 f = liftM2 f arbitrary arbitrary


liftArby3 :: (Arbitrary a, Arbitrary b, Arbitrary c) => (a -> b -> c -> d) -> Gen d
liftArby3 f = liftM3 f arbitrary arbitrary arbitrary


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




newtype MultiOp = ArbyMultiOp { getMultiOp :: Op }




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
                , OpNot
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


instance (Arbitrary a) => Arbitrary (BindEnv LangIdent a) where
    arbitrary = liftArby bindEnvFromList
    shrink (BindEnv x) = shrink1 bindEnvFromList (M.toList x)


instance (Arbitrary a) => Arbitrary (VarVal a) where
    arbitrary = liftArby2 VarVal
    shrink (VarVal x y) = shrink2 VarVal x y


instance Arbitrary Scope where
    arbitrary = liftArby3 Scope
    shrink (Scope w x y) = shrink3 Scope w x y


-- | Extracts a property from monadic Either code, giving
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
runEx s = case evalParse s program of
            Left e -> fail ("Could not parse string: " ++ s ++ "\n" ++ show e)
            Right r -> run $ runExec $ execStmt r
-- let (Right r) = evalParse s program in run $ runExec $ execStmt r


instance Arbitrary LangType where
    arbitrary = elements langTypes
    shrink x = filter (/=x) langTypes


langTypes :: [LangType]
langTypes = [LTStr, LTInt, LTFloat, LTList
            , LTBool, LTRange, LTNull ]


instance Arbitrary SourceRef where
    arbitrary = do
      name <- suchThat arbitrary (all (`elem` ['a'..'z']))
      (col1, line1) <- arbitrary
      (col2, line2) <- arbitrary
      -- start <- arbitrary
      -- end <- arbitrary
      return $ SourceRef (newPos name col1 line1, newPos name col2 line2)
    -- shrink (SourceRef x) = shrink1 SourceRef x


-- instance Arbitrary SourcePos where
--     arbitrary = do
--       f <- liftArby getPositive
--       s <- liftArby getPositive
--       t <- liftArby getPositive
--       return $ SourcePos (f, s, t)
--     shrink (SourcePos x) = shrink1 SourcePos x

assertEqual :: (Monad m, Eq a) => a -> a -> PropertyM m ()
assertEqual x = assert . (==x)


maxSized :: (Testable prop) => Int -> prop -> Property
maxSized x = mapSize (min x)


isValidIdent :: String -> Bool
isValidIdent [] = False
isValidIdent xs | xs `elem` keywords = False
isValidIdent (x:xs) = isAlpha x && all isAlphaNum xs
