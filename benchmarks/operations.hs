import Criterion.Main
import Criterion.Types

import Angle.Types.Lang
import Angle.Exec.Operations
import Angle.Exec.Types.Internal


main :: IO ()
main = defaultMainWith config
    [ bgroup "addition"
      [ bench "list of 1..10" $ whnf execSum [1..10]
      , bench "list of 1..100" $ whnf execSum [1..100]
      -- , bench "list of 1..1000" $ whnf lexListInt [1..1000]
      ]
    ]
  where
    config = defaultConfig { timeLimit = 1 }


execSum xs = runExecIOBasic $ addLit $ map LitInt xs
