module Main where

import Data.ModelTree
import Action
import Criterion.Main
import qualified Data.Text as T
import Model

exampleBenches = map benchme ["write", "copy", "an error", "manywrite"]
  where benchme s = bench s (whnf (runAction "Example" [MText (T.pack s)]) (Data.ModelTree.empty))

bench1000 = bench "1k" (whnf (o 1000) (Data.ModelTree.empty))
  where o 0 t = t :: ModelTree
        o n t = case runAction "Example" [MText (T.pack "write")] t of
                  Right (_, t', _) -> o (n-1 :: Int) t'
                  _ -> error ""

main = defaultMain [bgroup "example action" exampleBenches, bench1000]

