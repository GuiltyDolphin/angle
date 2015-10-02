module Test.Angle.Exec.Types
  ( tests
  ) where


import TestHelper

import Angle.Exec.Types.Internal


-- testFromIterInt :: NonNegative Int -> NonNegative Int -> NonZero Int -> Property
testFromIterInt :: RangeTriple Int -> Property
-- testFromIterInt (NonNegative x) (NonNegative y) (NonZero z) = monadicIO $ run (runExec (fromIter range)) >>= assertEqual expect
testFromIterInt (RangeTriple (x,y,z)) = monadicIO $ run (runExec (fromIter range)) >>= assertEqual expect
  where range = LitRange (LitInt x) (Just $ LitInt y) (Just $ LitInt z)
        expect = map LitInt [x,z..y]


newtype RangeTriple a = RangeTriple { getRangeTriple :: (a, a, a) }
  deriving (Show, Eq, Ord)


instance (Enum a, Arbitrary a) => Arbitrary (RangeTriple a) where
    arbitrary = liftM RangeTriple $ suchThat arbitrary checkRange
      where
        checkRange (x1,y1,z1)
          | z' == 0 = False
          | x' > y' && z' < 0 = True
          | x' < y' && z' > 0 = True
          | x' == y' = True
          | otherwise = False
          where [x', y', z'] = map fromEnum [x1,y1,z1]

tests :: [TestTree]
tests = [ testProperty "testFromIter - range int int int" testFromIterInt
        ]