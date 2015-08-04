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
    ) where

    
import Control.Applicative ((<*>), (<$>))
import Control.Monad (liftM, liftM2, liftM3)
import Control.Monad.Trans.Except
import Control.Monad.State
import Data.Char (isAlpha, isAlphaNum)
import Data.List (isInfixOf)
import qualified Data.Map as M

import Test.QuickCheck
import Test.QuickCheck.Monadic hiding (assert)
import qualified Test.QuickCheck.Monadic as Monadic
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Angle.Lex.Helpers (evalScan, Scanner)
import Angle.Lex.Token (keywords)
import Angle.Parse.Scope
import Angle.Parse.Error
import Angle.Parse.Types
import Angle.Scanner (SourcePos(..))
import Angle.Types.Lang


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
    shrink (LitList xs) = map LitList (shrink xs)
    shrink (LitInt x) = map LitInt (shrink x)
    shrink (LitStr x) = map LitStr (shrink x)
    shrink (LitFloat x) = map LitFloat (shrink x)
    shrink (LitBool x) = map LitBool (shrink x)
    --shrink (LitRange x y z) = zipWith3 LitRange (shrink x) (shrink y) (shrink z)
    shrink LitNull = [LitNull]
                            
                
instance Arbitrary SingStmt where
    arbitrary = frequency 
                [ (7, liftArby2 StmtAssign)
                , (2, liftArby StmtStruct)
                , (5, liftArby StmtExpr)
                , (8, liftArby StmtReturn)
                ]
    shrink (StmtAssign x y) = 
        zipWith StmtAssign (shrink x) (shrink y)
    shrink (StmtStruct x) = map StmtStruct (shrink x)
    shrink (StmtExpr x) = map StmtExpr (shrink x)
    shrink (StmtReturn x) = map StmtReturn (shrink x)

    
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


instance Arbitrary Lambda where
    arbitrary = do
      args <- arbitrary
      body <- arbitrary
      typ <- arbitrary
      return Lambda { lambdaArgs=args, lambdaBody=body, lambdaType=typ }
    shrink (Lambda x y z) = zipWith3 Lambda (shrink x) (shrink y) (shrink z)
             
    
instance Arbitrary Expr where
    arbitrary = frequency 
                [ (15, liftArby ExprIdent)
                , (9, liftArby  ExprLit)
                , (1, liftM2 ExprFunCall arbitrary (liftArby getTinyList))
                , (4, liftArby  ExprOp)
                --, (4, liftArby ExprLambda)
                , (4, liftArby ExprFunIdent)
                ]
    shrink (ExprIdent x) = map ExprIdent (shrink x)
    shrink (ExprLit x) = map ExprLit (shrink x)
    shrink (ExprFunCall x y) = zipWith ExprFunCall (shrink x) (shrink y)
    shrink (ExprOp x) = map ExprOp (shrink x)
    shrink (ExprLambda x) = map ExprLambda (shrink x)
    shrink (ExprFunIdent x) = map ExprFunIdent (shrink x)
    shrink (ExprList x) = map ExprList $ shrink x


instance Arbitrary ArgSig where
    arbitrary = do
      args <- liftArby getTinyList
      catchArg <- arbitrary
      return ArgSig { Angle.Types.Lang.stdArgs = args, catchAllArg = catchArg }
    shrink (ArgSig x y) = zipWith ArgSig (shrink x) (shrink y)
             
   
instance Arbitrary ArgElt where
    arbitrary = liftArby3 ArgElt
                
instance Arbitrary ClassRef where
    arbitrary = liftArby ClassRef
                
instance Arbitrary AnnType where
    arbitrary = elements [AnnFun, AnnClass, AnnLit]
                
instance Arbitrary LambdaType where
    arbitrary = elements [FunLambda, ClassLambda]

instance Arbitrary Stmt where
    arbitrary = frequency
                [ (9, liftArby2 SingleStmt)
                , (1, liftM MultiStmt (liftArby getTinyList))
                ]
    shrink (SingleStmt x p) = zipWith SingleStmt (shrink x) (shrink p)
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
    arbitrary = oneof [ liftArby getMultiOp
                      , liftArby getSpecOp]



liftArby :: (Arbitrary a) => (a -> b) -> Gen b
liftArby f = liftM f arbitrary


liftArby2 :: (Arbitrary a, Arbitrary b) => (a -> b -> c) -> Gen c
liftArby2 f = liftM2 f arbitrary arbitrary


liftArby3 :: (Arbitrary a, Arbitrary b, Arbitrary c) => (a -> b -> c -> d) -> Gen d
liftArby3 f = liftM3 f arbitrary arbitrary arbitrary
              

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


isValidIdent :: String -> Bool
isValidIdent [] = False
isValidIdent x | x `elem` keywords = False
               | otherwise = isAlpha (head x) && all isAlphaNum (drop 1 x)

                 
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

    
newtype ValidComment = ValidComment { getValidComment :: String }
    

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
    shrink (SmallList xs) = map SmallList (shrink xs)


instance (Arbitrary a) => Arbitrary (TinyList a) where
    arbitrary = sized $ \s -> do
                  n <- choose (0,s`min`maxTinyListLength)
                  xs <- vector n
                  return (TinyList xs)
    shrink (TinyList xs) = map TinyList (shrink xs)
             

instance (Arbitrary a) => Arbitrary (BindEnv a) where
    arbitrary = liftArby bindEnvFromList
    

instance (Arbitrary a) => Arbitrary (VarVal a) where
    arbitrary = liftArby2 VarVal
    shrink (VarVal x y) = zipWith VarVal (shrink x) (shrink y)


instance Arbitrary Scope where
    arbitrary = liftArby4 Scope


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
    Left _ -> fail "meh"
    Right r -> return r

-- monadicExec :: PropertyM ExecIO a -> PropertyM ExecIO (IO (Either AngleError a))
-- monadicExec e = do
--  x <- e
--  return (runExecIOBasic x)
  
  
                    
                  

instance Arbitrary LangType where
    arbitrary = elements [LTStr, LTInt, LTFloat, LTList
                         , LTBool, LTRange, LTNull ]
                

instance Arbitrary SourceRef where
    arbitrary = do
      start <- arbitrary
      end <- arbitrary
      return $ SourceRef (start, end)
             

instance Arbitrary SourcePos where
    arbitrary = do
      f <- liftArby getPositive
      s <- liftArby getPositive
      t <- liftArby getPositive
      return $ SourcePos (f, s, t)
                

assertEqualQC :: (Monad m, Eq a) => a -> a -> PropertyM m ()
assertEqualQC x = assertQC . (==x)


isNumeric :: LangLit -> Bool
isNumeric x = case typeOf x of
                LTFloat -> True
                LTInt   -> True
                _       -> False


maxSized :: (Testable prop) => Int -> prop -> Property
maxSized x = mapSize (min x)


assertQC :: (Monad m) => Bool -> PropertyM m ()
assertQC = Monadic.assert
