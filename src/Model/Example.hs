{-# LANGUAGE DeriveGeneric #-}
module Model.Example where

import Data.Serialize
import Test.QuickCheck(Arbitrary, arbitrary)
import GHC.Generics (Generic)
import Control.Monad (liftM)

data Example = Example String
     deriving (Eq, Ord, Show, Generic)

instance Serialize Example

instance Arbitrary Example where
  arbitrary = liftM Example arbitrary
