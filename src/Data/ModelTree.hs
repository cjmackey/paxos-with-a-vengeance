{-# LANGUAGE DeriveGeneric #-}
module Data.ModelTree where

import Control.Monad(forM, forM_)
import Data.Serialize
import Data.Text(Text)
import Data.ByteString(ByteString)
import qualified Data.Text.Encoding
import qualified Data.Map as M
import Test.QuickCheck(Arbitrary)
import GHC.Generics (Generic)
import Model

data ModelTree = ModelTree Model (M.Map Text ModelTree)
    deriving (Eq, Ord, Show, Generic)

instance Serialize ModelTree where
  put (ModelTree model childmap) = do
    put model
    let mapping = M.toAscList childmap
    put (fromIntegral (length mapping) :: Integer)
    forM_ mapping (\ (k, _) -> put (Data.Text.Encoding.encodeUtf8 k))
    forM_ mapping (\ (_, t) -> put t)
  get = do
    model <- get :: Get Model
    len <- get :: Get Integer
    bkeys <- forM [1..len] (\ _ -> get :: Get ByteString)
    let keys = map Data.Text.Encoding.decodeUtf8 bkeys
    children <- forM [1..len] (\ _ -> get :: Get ModelTree)
    let childmap = M.fromAscList (zip keys children)
    return (ModelTree model childmap)

instance Arbitrary ModelTree
