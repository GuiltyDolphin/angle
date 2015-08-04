module Test.Angle.Parse.Interp
    ( tests
    ) where

import Control.Monad
import System.Process
import System.IO
    
import TestHelper
import Angle.Parse.Interp


examplePath = angleDir ++ "tests/examples/"
          
interpPath = angleDir ++ "src/Angle/Parse/Interp.hs"
          
angleDir = "/home/ben/Dropbox/programming/haskell/other/scan1/AngleProj/"
          

runner :: String -> [String] -> IO String
runner p ags = do
  let toRun = [interpPath, p] ++ ags
  (_,Just hout,_, _) <- createProcess(proc "runhaskell" toRun){std_out=CreatePipe,cwd=Just $ angleDir++"src"}
  --hPutStrLn hout "test"
  -- return hout
  --readCreateProcessWithExitCode (proc "runhaskell" toRun){cwd="/home/ben/Dropbox/programming/haskell/other/scan1/AngleProj/src"} ""
  --readProcess "runhaskell" toRun ""
  hGetContents hout
           

fileQS = examplePath ++ "quickSort"


testQuickSortInt :: [Int] -> Property
testQuickSortInt xs = monadicIO $ do 
  sorted <- liftM read $ run $ runner fileQS [(show xs)]
  assertEqualQC xs sorted


tests = [ testGroup "quickSort"
          [ -- localOption (QuickCheckMaxSize 2) . localOption (Timeout (6*(10^6)) "") . localOption (QuickCheckTests 10) $ testProperty "int" testQuickSortInt
          ] 
        ]
