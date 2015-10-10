import Criterion.Main
import Criterion.Types

import Angle.Types.Lang
import Angle.Exec.Operations
import Angle.Exec.Types.Internal


main :: IO ()
main = defaultMainWith config
    [ bgroup "addition"
      [ bench "list of 1..10" $! nfIO $! execSum [1..10]
      , bench "list of 1..100" $! nfIO $! execSum [1..100]
      -- , bench "list of 1..1000" $ whnf lexListInt [1..1000]
      ]
    , bgroup "logical and"
      [ bench "list of length 10" $! nfIO $! execAnd 10
      , bench "list of length 100" $! nfIO $! execAnd 100
      , bench "list of length 1000" $! nfIO $! execAnd 1000
      ]
    , bgroup "appending to list"
      [ bench "list of 1..10" $! whnf execAppend [1..10]
      , bench "list of 1..100" $! whnf execAppend [1..100]
      ]
    ]
  where
    config = defaultConfig { timeLimit = 1 }


execSum xs = fromInt' $! addLit $! map LitInt xs
execAnd n = fromBool $! andLit $! map LitBool $! replicate n True ++ [False]
execAppend xs = fromList $! addLit $! LitList [] : map LitInt xs


withRes :: ExecIO LangLit -> IO LangLit
withRes s = do
  (Right r) <- runExecIOBasic s
  return r

fromInt' :: ExecIO LangLit -> IO Int
fromInt' s = do
  (LitInt r) <- withRes s
  return r

fromBool :: ExecIO LangLit -> IO Bool
fromBool s = do
  (LitBool r) <- withRes s
  return r

fromList :: ExecIO LangLit -> IO [LangLit]
fromList s = do
  (LitList r) <- withRes s
  return r
