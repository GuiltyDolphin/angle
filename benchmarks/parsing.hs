import Angle.Parse.Parser.Internal

import Criterion.Main
import Criterion.Types

main :: IO ()
main = defaultMainWith config
    [ bgroup "literals"
      [ bench "list of 1..10" $ whnf parseListInt [1..10]
      , bench "list of 1..100" $ whnf parseListInt [1..100]
      --, bench "list of 1..1000" $ whnf parseListInt [1..1000]
      ]
    ]
  where
    config = defaultConfig { timeLimit = 1 }


parseListInt xs = evalParse (show xs) langLit
