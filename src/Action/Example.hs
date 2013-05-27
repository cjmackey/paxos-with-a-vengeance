module Action.Example where

import Model
import Data.ModelTree(ModelTree, TreeMod, treeMod)

act :: [Model] -> ModelTree -> Either String [TreeMod]
act args mt0 = if length args == 1
               then Left "omg args"
               else Right [treeMod "some/path" (MInteger 42)]