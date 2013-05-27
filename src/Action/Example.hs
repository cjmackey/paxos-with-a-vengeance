module Action.Example where

import Model
import Data.ModelTree(ModelTree, TreePath, TreeMod)

act :: [Model] -> ModelTree -> Either String [TreeMod]
