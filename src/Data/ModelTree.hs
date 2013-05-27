{-# LANGUAGE DeriveGeneric, TypeSynonymInstances, FlexibleInstances #-}
module Data.ModelTree where

import Prelude hiding (lookup)
import Control.Monad(forM, forM_)
import Data.Serialize
import Data.Text(Text)
import qualified Data.Text as T
import Data.ByteString(ByteString)
import qualified Data.Text.Encoding
import qualified Data.Map as M
import Test.QuickCheck(Arbitrary)
import GHC.Generics (Generic)
import Data.Maybe(fromMaybe)

import Model(Model(MNothing))

data ModelTree = ModelTree Model (M.Map Text ModelTree)
    deriving (Eq, Ord, Show, Generic)
empty = ModelTree MNothing M.empty

lookup :: (TreePathLike p) => p -> ModelTree -> Maybe ModelTree
lookup p0 = l (toTreePath p0)
  where l [] mt = Just mt
        l (h:p) (ModelTree _ children) = M.lookup h children >>= l p
--          case M.lookup h children of
--            Just child -> l p child
--            Nothing -> Nothing

lookup' :: (TreePathLike p) => p -> ModelTree -> Model
lookup' p mt = case lookup p mt of
                 Just (ModelTree m _) -> m
                 Nothing -> MNothing
insert :: (TreePathLike p) => p -> Model -> ModelTree -> ModelTree
insert p m = applyTreeMod (treeMod p m)

applyTreeMod :: TreeMod -> ModelTree -> ModelTree
applyTreeMod ([], m) (ModelTree _ children) = ModelTree m children
applyTreeMod (p0:p, m) (ModelTree m0 children) =
  let child = fromMaybe empty $ M.lookup p0 children
      child' = applyTreeMod (p, m) child
      children' = M.insert p0 child' children
  in ModelTree m0 children'

applyTreeMods :: [TreeMod] -> ModelTree -> ModelTree
applyTreeMods l mt = foldl (flip applyTreeMod) mt l

type TreePath = [Text]
type TreeMod = (TreePath, Model)

treeMod :: (TreePathLike p) => p -> Model -> TreeMod
treeMod p m = (toTreePath p, m)

class TreePathLike a where
  toTreePath :: a -> TreePath

instance TreePathLike TreePath where
  toTreePath = id

instance TreePathLike [String] where
  toTreePath = map T.pack

instance TreePathLike Text where
  toTreePath t = filter (\x -> T.length x > 0) $ T.splitOn (T.pack "/") t

instance TreePathLike String where
  toTreePath = toTreePath . T.pack



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
