module TestHelper
    ( --module Test.Framework
    -- , module Test.Framework.Providers.HUnit
    -- , module Test.Framework.Providers.QuickCheck2
      module Test.Tasty
    , module Test.Tasty.QuickCheck
    , module Test.Tasty.HUnit
    , Scanner
    , evalScan
    ) where

    
import Control.Monad (liftM, liftM2, liftM3)
import Control.Applicative ((<*>), (<$>))
import Data.Char (isAlpha, isAlphaNum)
import Data.List (isInfixOf)

import Test.Tasty.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
-- import Test.Framework.Providers.HUnit
-- import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Angle.Types.Lang
import Angle.Lex.Helpers (evalScan, Scanner)

instance Arbitrary LangLit where
    arbitrary = frequency 
                [ (4, liftArby (LitStr . getValidLitStr))
                , (6, liftArby LitInt)
                , (6, liftArby LitFloat)
                , (1, liftM LitList (liftArby getSmallList))
                , (6, liftArby LitBool)
                , (1, liftArby2 LitRange)
                , (6, return LitNull)
                ]
    shrink (LitList xs) = map LitList (shrink xs)
    shrink (LitInt x) = map LitInt (shrink x)
    shrink (LitStr x) = map LitStr (shrink x)
    shrink (LitFloat x) = map LitFloat (shrink x)
    shrink (LitBool x) = map LitBool (shrink x)
    shrink (LitRange x y) = zipWith LitRange (shrink x) (shrink y)
                            

                
instance Arbitrary SingStmt where
    arbitrary = frequency 
                [ (7, liftArby2 StmtAssign)
                , (6, liftM StmtComment (liftArby getValidComment))
                , (3, liftArby StmtStruct)
                , (4, liftArby StmtExpr)
                ]
    shrink (StmtAssign x y) = zipWith StmtAssign (shrink x) (shrink y)
    shrink (StmtComment x) = map StmtComment (filter validComment $ shrink x)
        where validComment x | '\n' `elem` x = False
                             | "-#" `isInfixOf` x = False
                             | otherwise = True
    shrink (StmtStruct x) = map StmtStruct (shrink x)
    shrink (StmtExpr x) = map StmtExpr (shrink x)

instance Arbitrary LangStruct where
    arbitrary = frequency 
                [ (3, liftArby3 StructFor)
                , (3, liftArby2 StructWhile)
                , (3, liftM3 StructIf arbitrary (liftArby MultiStmt) arbitrary)
                , (1, liftArby2 StructDefun)
                -- , liftArby StructReturn -- Not using atm
                ]
    shrink (StructFor x y z) = zipWith3 StructFor (shrink x) (shrink y) (shrink z)
    shrink (StructWhile x y) = zipWith StructWhile (shrink x) (shrink y)
    shrink (StructIf x y z) = zipWith3 StructIf (shrink x) (shrink y) (shrink z)
    shrink (StructDefun x y) = zipWith StructDefun (shrink x) (shrink y)

instance Arbitrary CallSig where
    arbitrary = do
      args <- arbitrary
      body <- arbitrary
      return CallSig { callArgs=args, callBody=body }
    shrink (CallSig x y) = zipWith CallSig (shrink x) (shrink y)
             
instance Arbitrary Expr where
    arbitrary = frequency 
                [ (9, liftArby ExprIdent)
                , (6, liftArby  ExprLit)
                , (1, liftM2 ExprFunCall arbitrary (liftArby getTinyList))
                , (4, liftArby  ExprOp)
                ]
    shrink (ExprIdent x) = map ExprIdent (shrink x)
    shrink (ExprLit x) = map ExprLit (shrink x)
    shrink (ExprFunCall x y) = zipWith ExprFunCall (shrink x) (shrink y)
    shrink (ExprOp x) = map ExprOp (shrink x)

instance Arbitrary ArgSig where
    arbitrary = do
      args <- liftArby getTinyList
      catchArg <- arbitrary
      return ArgSig { Angle.Types.Lang.stdArgs = args, catchAllArg = catchArg }
    shrink (ArgSig x y) = zipWith ArgSig (shrink x) (shrink y)
             
instance Arbitrary Stmt where
    arbitrary = frequency
                [ (9, liftArby SingleStmt)
                , (1, liftM MultiStmt (liftArby getTinyList))
                ]
    shrink (SingleStmt x) = map SingleStmt (shrink x)
    shrink (MultiStmt xs) = map MultiStmt (shrink xs)

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
                  
    shrink (SpecOp x y) = zipWith SpecOp (shrink x) (shrink y)
    shrink (MultiOp x ys) = zipWith MultiOp (shrink x) (shrink ys)

instance Arbitrary Op where
    arbitrary = elements
                [ OpNeg
                , OpMult
                , OpDiv
                , OpAdd
                , OpSub
                , OpNot
                , OpEq
                ]



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
    arbitrary = (liftM LangIdent) $ 
                (:) 
                <$> chooseAlpha 
                <*> listOf chooseAlphaNum
    shrink (LangIdent x) = map LangIdent (filter isValidIdent (shrink x))
        where 
          isValidIdent [] = False
          isValidIdent x = isAlpha (head x) && all isAlphaNum (drop 1 x)
                
chooseAlpha = oneof [choose ('a','z'), choose ('A','Z')]
chooseDigit = choose ('0','9')
chooseAlphaNum = oneof [chooseAlpha, chooseDigit]

                 
newtype SpecOp = ArbySpecOp { getSpecOp :: Op }
newtype MultiOp = ArbyMultiOp { getMultiOp :: Op }
    
instance Arbitrary SpecOp where
    arbitrary = elements $ map ArbySpecOp [OpNeg, OpNot]
                
instance Arbitrary MultiOp where
    arbitrary = elements $ map ArbyMultiOp
                [ OpMult
                , OpDiv
                , OpAdd
                , OpSub
                , OpEq
                ]

newtype ValidComment = ValidComment { getValidComment :: String }
    
instance Arbitrary ValidComment where
    arbitrary = liftM ValidComment $ arbitrary `suchThat` isValidComment
      --a <- arbitrary
      --if isValidComment a
      --  then return $ ValidComment a
      --  else arbitrary
            where isValidComment x | '\n' `elem` x = False
                                   | "-#" `isInfixOf` x = False
                                   | otherwise = True

maxSmallListLength = 50         
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
    shrink (SmallList xs) = map SmallList (shrink xs)

instance (Arbitrary a) => Arbitrary (TinyList a) where
    arbitrary = sized $ \s -> do
                  n <- choose (0,s`min`maxTinyListLength)
                  xs <- vector n
                  return (TinyList xs)
    shrink (TinyList xs) = map TinyList (shrink xs)
