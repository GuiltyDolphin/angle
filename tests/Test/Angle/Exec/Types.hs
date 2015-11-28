module Test.Angle.Exec.Types
  ( tests
  ) where


import TestHelper

import Angle.Exec.Types.Internal


testFromIterInt :: RangeTriple Int -> Property
testFromIterInt (RangeTriple (x,y,z)) = monadicIO $ run (runExec (fromIter range)) >>= assertEqual expect
  where range = LitRange (LitInt x) (Just $ LitInt y) (Just $ LitInt z)
        expect = map LitInt [x,z..y]


newtype RangeTriple a = RangeTriple (a, a, a)
  deriving (Show, Eq, Ord)


instance (Enum a, Arbitrary a) => Arbitrary (RangeTriple a) where
    arbitrary = liftM RangeTriple $ suchThat arbitrary checkRange
      where
        checkRange (x1,y1,z1) = not ((y' > x' || y' == x') && (z' == x'))
          where
            [x', y', z'] = map fromEnum [x1,y1,z1]


tests :: [TestTree]
tests = [ testProperty "testFromIter - range int int int" testFromIterInt
        ]
