model Update where

import Data.Serialize

import Model (Model)

import Data.ModelTree (TreePath, ModelTree)

data Update = ModelUpdate TreePath Model
            | TreeUpdate TreePath ModelTree
  deriving (Eq, Ord, Show, Generic)

instance Serialize Update
