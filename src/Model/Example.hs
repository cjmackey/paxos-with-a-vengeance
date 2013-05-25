{-# LANGUAGE DeriveGeneric #-}
module Model.Example where

import Data.Serialize
import GHC.Generics (Generic)

data Example = Example String
     deriving (Eq, Ord, Show, Generic)
instance Serialize Example

{-
instance Serialize Example where
  put (Example s) = put s
  get = do
    s <- get
    return (Example s)
-}

