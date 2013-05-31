{-# LANGUAGE DeriveGeneric #-}
module Data.Protocol.Command (Command(Command), runCommand) where

import Action (runAction)
import Model (Model)
import Data.ModelTree (ModelTree, TreeMod)
import GHC.Generics (Generic)
import Data.Sequence (Seq)
import Data.Serialize

data Command = Command String [Model]
  deriving (Eq, Ord, Show, Generic)
instance Serialize Command

runCommand :: Command -> ModelTree -> Either String (Model, ModelTree, Seq TreeMod)
runCommand (Command s l) = runAction s l


