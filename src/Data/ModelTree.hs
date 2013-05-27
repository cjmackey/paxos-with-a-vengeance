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
mtChildren (ModelTree _ children) = children

lookup :: (TreePathLike p) => p -> ModelTree -> Maybe ModelTree
lookup p0 = l (toTreePath p0)
  where l [] mt = Just mt
        l (h:p) (ModelTree _ children) = M.lookup h children >>= l p

lookupModel :: (TreePathLike p) => p -> ModelTree -> Model
lookupModel p mt = case lookup p mt of
                 Just (ModelTree m _) -> m
                 Nothing -> MNothing
insertModel :: (TreePathLike p) => p -> Model -> ModelTree -> ModelTree
insertModel p m = applyTreeMod (InsModel (toTreePath p) m)

applyTreeMod :: TreeMod -> ModelTree -> ModelTree
applyTreeMod (InsModel [] m) (ModelTree _ children) = ModelTree m children
applyTreeMod (InsModel (p0:p) m) (ModelTree m0 children) =
  let child = fromMaybe empty $ M.lookup p0 children
      child' = applyTreeMod (InsModel p m) child
      children' = M.insert p0 child' children
  in ModelTree m0 children'
applyTreeMod (Del []) x = x
applyTreeMod (Del [p]) (ModelTree m children) = ModelTree m (M.delete p children)
applyTreeMod (Del (p0:p)) (ModelTree m children) = ModelTree m (M.adjust (applyTreeMod (Del p)) p0 children)
applyTreeMod (Copy source target) mt = maybe mt (\node -> applyTreeMod (InsTree target node) mt) (lookup source mt)
applyTreeMod (InsTree [] n) mt = n
applyTreeMod (InsTree (p0:p) n) (ModelTree m0 children) =
  let child = fromMaybe empty $ M.lookup p0 children
      child' = applyTreeMod (InsTree p n) child
      children' = M.insert p0 child' children
  in ModelTree m0 children'

applyTreeMods :: [TreeMod] -> ModelTree -> ModelTree
applyTreeMods l mt = foldl (flip applyTreeMod) mt l

type TreePath = [Text]
data TreeMod = InsModel TreePath Model
             | InsTree TreePath ModelTree
             | Copy TreePath TreePath
             | Del TreePath
  deriving (Eq, Ord, Show)

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
