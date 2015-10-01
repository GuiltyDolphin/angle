module Test.Angle.Exec.Types
  ( tests
  ) where


import TestHelper

import Angle.Exec.Types.Internal


testFromIterInt :: NonNegative Int -> NonNegative Int -> NonZero Int -> Property
testFromIterInt (NonNegative x) (NonNegative y) (NonZero z) = monadicIO $ run (runExec (fromIter range)) >>= assertEqual expect
  where range = LitRange (LitInt x) (Just $ LitInt y) (Just $ LitInt z)
        expect = map LitInt [x,z..y]


tests :: [TestTree]
tests = [ testProperty "testFromIter - range int int int" testFromIterInt
        ]
