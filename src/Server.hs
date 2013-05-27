module Main where

import Data.NodeState
import Network (withSocketsDo)

main = withSocketsDo $ do
  Data.NodeState.run Data.NodeState.empty

