module Control.Monad.Act (Act, nodeExists, children, stat, read, write, delete, copy, runAct) where

import Prelude hiding (read, mod, lookup)

--import qualified Data.Map as M
import Data.Map(Map)

--import qualified Data.Text as T
import Data.Text(Text)

import Data.ModelTree hiding (applyTreeMod)
import qualified Data.ModelTree
import Model(Model)

import Data.Maybe(isJust)

import Data.Sequence

import Control.Monad.State

type Act = State ActState

data ActState = ActState ModelTree (Seq TreeMod)
  deriving (Eq, Ord, Show)
tree (ActState mt _) = mt
mods (ActState _ ml) = ml
actState mt = ActState mt Data.Sequence.empty

runAct :: Act (Either e o) -> ModelTree -> Either e (o, ModelTree, Seq TreeMod)
runAct act mt =
  let (o, as') = runState act as
      as = actState mt
  in case o of
       Right x -> Right (x, tree as', mods as')
       Left err -> Left err

nodeExists :: (TreePathLike p) => p -> Act Bool
nodeExists p = do
  as <- get
  return (isJust (Data.ModelTree.lookup p (tree as)))

read :: (TreePathLike p) => p -> Act Model
read p = do
  as <- get
  return (lookupModel p (tree as))

stat :: (TreePathLike p) => p -> Act (Maybe ModelTree)
stat p = do
  as <- get
  return (lookup p (tree as))

children :: (TreePathLike p) => p -> Act (Maybe (Map Text ModelTree))
children p = do
  mmt <- stat p
  return (case mmt of
            Just mt -> Just (mtChildren mt)
            Nothing -> Nothing)

write :: (TreePathLike p) => p -> Model -> Act ()
write p m = do
  let mod = InsModel (toTreePath p) m
  applyTreeMod mod

copy p1 p2 = do
  let mod = Copy (toTreePath p1) (toTreePath p2)
  applyTreeMod mod

delete p = do
  let mod = Del (toTreePath p)
  applyTreeMod mod

applyTreeMod mod = do
  as <- get
  let as' = ActState (Data.ModelTree.applyTreeMod mod (tree as)) (mods as |> mod)
  put as'
  return ()


