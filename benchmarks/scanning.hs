import Angle.Scanner (scanChar, evalScan)

import Criterion.Main
import Criterion.Types

main :: IO ()
main = defaultMainWith config
    [ bgroup "basic scanning"
      [ bgroup "single character"
        [ bench "stream length 1" $ whnf scanSingleLength 1
        , bench "stream length 100" $ whnf scanSingleLength 100
        , bench "stream length 1000" $ whnf scanSingleLength 1000
        -- , bench "list of 1..1000" $ whnf lexListInt [1..1000]
        ]
      ]
    ]
  where
    config = defaultConfig { timeLimit = 1 }

scanSingleLength n = evalScan (replicate n 'A') scanChar
