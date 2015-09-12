import Angle.Exec.Exec
import Angle.Parse.Parser.Internal

import Criterion.Main
import Criterion.Types

main :: IO ()
main = defaultMainWith config
    [ bgroup "sums"
      [ bench "list of 1..10" $ whnf execSum [1..10]
      , bench "list of 1..100" $ whnf execSum [1..100]
      -- , bench "list of 1..1000" $ whnf lexListInt [1..1000]
      ]
    ]
  where
    config = defaultConfig { timeLimit = 1 }


lexListInt xs = evalScan (show xs) langLit
