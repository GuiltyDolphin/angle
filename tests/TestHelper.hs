{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module TestHelper
    ( module Test.Tasty
    , module Test.Tasty.QuickCheck
    , module Angle.Types.Lang
    , module Control.Applicative
    , module Data.Maybe
    , assert
    , Parser
    , evalParse
    , monadicEither
    , liftM
    , liftArby
    , monadicIO
    , assertEqual
    , run
    , maxSized
    , runExecIOBasic
    , runExec
    , runEx
    , runExBuiltin
    , SmallList(..)
    , TinyList(..)
    ) where


import Control.Applicative ((<*>), (<$>), pure)
import Control.Monad.State
import Data.Char (isAlpha, isAlphaNum)
import Data.Maybe (isNothing)
import qualified Data.Map as M

import Text.Parsec.Pos

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck

import Angle.Parse.Helpers (evalParse, Parser)
import Angle.Parse.Token
import Angle.Exec.Types.Internal
import Angle.Types.Lang
import Angle.Types.Scope
import Angle.Parse.Parser (program)
import Angle.Exec.Exec (execStmt)
import Angle.Exec.Builtins (initialEnvNotMain)


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
    shrink (LitStr x) = LitStr <$> shrink x
    shrink (LitInt x) = LitInt <$> shrink x
    shrink (LitFloat x) = LitFloat <$> shrink x
    shrink (LitList xs) = LitList <$> shrink xs
    shrink (LitBool x) = LitBool <$> shrink x
    shrink (LitRange x y z) = LitRange <$> shrink x <*> shrink y <*> shrink z
    shrink (LitChar x) = LitChar <$> shrink x
    shrink (LitLambda x) = LitLambda <$> shrink x


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
                , (3, liftM3 StructIf arbitrary (liftArby MultiStmt) arbitrary)
                , (1, liftArby2 StructDefun)
                ]
    shrink (StructFor x y z) = shrink3 StructFor x y z
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
                ]
    shrink (ExprIdent x) = ExprIdent <$> shrink x
    shrink (ExprLit x) = ExprLit <$> shrink x
    shrink (ExprFunCall f b xs) = ExprFunCall <$> shrink f <*> shrink b <*> shrink xs


instance Arbitrary ArgSig where
    arbitrary = do
      args <- liftArby getTinyList
      catchArg <- arbitrary
      return ArgSig { Angle.Types.Lang.stdArgs = args, catchAllArg = catchArg }
    shrink (ArgSig x y) = shrink2 ArgSig x y


instance Arbitrary ArgElt where
    arbitrary = liftArby2 ArgElt
    shrink (ArgElt x y) = shrink2 ArgElt x y


instance Arbitrary CatchArg where
    arbitrary = liftArby2 CatchArg
    shrink (CatchArg x y) = CatchArg <$> shrink x <*> shrink y


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


instance Arbitrary Stmt where
    arbitrary = frequency
                [ (9, liftArby2 SingleStmt)
                , (1, liftM MultiStmt (liftArby getTinyList))
                ]
    shrink (SingleStmt x p) = shrink2 SingleStmt x p
    shrink (MultiStmt xs) = shrink1 MultiStmt xs


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
  arbitrary = liftM LangIdent $ frequency
                                [ (15, validIdent `suchThat` isValidIdent)
                                , (2, symbolIdent)
                                ]
  shrink (LangIdent x) = map LangIdent (filter isValidIdent (shrink x))


newtype SymbolIdentChar = SymbolIdentChar { getSymbolIdentChar :: Char }


instance Arbitrary SymbolIdentChar where
  arbitrary = SymbolIdentChar <$> elements validSymbolIdentChars
  shrink (SymbolIdentChar c) = SymbolIdentChar <$> filter (/= c) validSymbolIdentChars


symbolIdent :: Gen String
symbolIdent = liftM (fmap getSymbolIdentChar . getTinyList) arbitrary `suchThat` (not . null)

validIdent :: Gen String
validIdent = (:) <$> chooseAlpha <*> listOf chooseAlphaNum
    where chooseAlpha = oneof [choose ('a','z'), choose ('A','Z')]
          chooseDigit = choose ('0','9')
          chooseAlphaNum = oneof [chooseAlpha, chooseDigit]


newtype BuiltInOp = BuiltInOp { getBuiltInOp :: LangIdent }


instance Arbitrary BuiltInOp where
  arbitrary = elements $ map (BuiltInOp . LangIdent) builtinOps


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
                  n <- frequency [ (5, choose (0, 3))
                                , (2, choose (4, 7))
                                , (1, choose (8, 10))
                                ]
                  xs <- vector n
                  return (TinyList xs)
    shrink (TinyList xs) = shrink1 TinyList xs


instance (Ord n, Arbitrary n, Arbitrary v) => Arbitrary (BindEnv n v) where
    arbitrary = liftArby bindEnvFromList
    shrink (BindEnv x) = shrink1 bindEnvFromList (M.toList x)


instance (Arbitrary a) => Arbitrary (VarVal a) where
    arbitrary = liftArby2 VarVal
    shrink (VarVal x y) = shrink2 VarVal x y


instance Arbitrary Scope where
    arbitrary = liftArby2 Scope
    shrink (Scope w x) = shrink2 Scope w x


-- | Extracts a property from monadic Either code, giving
-- a failing property if the result is a Left.
monadicEither :: PropertyM (Either e) a -> Property
monadicEither = monadic (\e -> case e of
                                 Left _ -> property False
                                 Right r -> r)



runExecEnv :: Env -> ExecIO a -> IO a
runExecEnv env e = do
  x <- runExecIOEnv env e
  case x of
    Left err -> fail $ "runExecBuiltin failed with: " ++ show err
    Right r -> return r

runExec :: ExecIO a -> IO a
runExec = runExecEnv basicEnv

runExecBuiltin :: ExecIO a -> IO a
runExecBuiltin = runExecEnv initialEnvNotMain

runExEnv :: Env -> String -> PropertyM IO LangLit
runExEnv env s = case evalParse s program of
            Left e -> fail ("Could not parse string: " ++ s ++ "\n" ++ show e)
            Right r -> run $ runExecEnv env $ execStmt r

runEx :: String -> PropertyM IO LangLit
runEx = runExEnv basicEnv

runExBuiltin :: String -> PropertyM IO LangLit
runExBuiltin = runExEnv initialEnvNotMain


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
    shrink (SourceRef (p1, p2)) =
      let name = sourceName p1
          line1 = sourceLine p1
          col1 = sourceColumn p1
          line2 = sourceLine p2
          col2 = sourceColumn p2
      in do
        (nline1, ncol1) <- shrink (line1, col1)
        (nline2, ncol2) <- shrink (line2, col2)
        let newPos1 = newPos name nline1 ncol1
            newPos2 = newPos name nline2 ncol2
        return $ SourceRef (newPos1, newPos2)
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
