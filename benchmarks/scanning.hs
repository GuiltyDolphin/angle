import Control.Monad

import Criterion.Main
import Criterion.Types

import Angle.Scanner (scanChar, evalScan)


main :: IO ()
main = defaultMainWith config
    [ bgroup "basic scanning"
      [ bgroup "single character"
        [ bench "stream length 1" $ whnf scanSingleLength 1
        , bench "stream length 100" $ whnf scanSingleLength 100
        , bench "stream length 1000" $ whnf scanSingleLength 1000
        ]
      , bgroup "full stream - single character"
        [ bench "stream length 10" $ whnf scanSingleLengthFull 10
        , bench "stream length 100" $ whnf scanSingleLengthFull 100
        , bench "stream length 1000" $ whnf scanSingleLengthFull 1000
        ]
      ]
    ]
  where
    config = defaultConfig { timeLimit = 1 }


scanSingleLength n = evalScan (replicate n 'A') scanChar

scanSingleLengthFull n = evalScan (replicate n 'A') (replicateM n scanChar)
