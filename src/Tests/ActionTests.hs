{-# LANGUAGE TemplateHaskell #-}
module Tests.ActionTests(testGroup) where

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck()

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Sequence as S

import Action
import Model
import Data.ModelTree

{-# ANN module "HLint: ignore Use camelCase" #-}

case_no_action = runAction "Not An Action" [] Data.ModelTree.empty @?= Left "no such action!? Not An Action"

case_example_return = runAction "Action.Example" [] Data.ModelTree.empty @?= Right (MInteger 42, Data.ModelTree.empty, S.empty)

case_example_write = runAction "Example" [MText (T.pack "write")] Data.ModelTree.empty @?= Right (MNothing, ModelTree MNothing (M.fromList [(T.pack "some", ModelTree MNothing (M.fromList [(T.pack "path", ModelTree (MInteger 42) M.empty)]))]), S.fromList [InsModel (toTreePath "some/path") (MInteger 42)])

case_example_copy = runAction "Example" [MText (T.pack "copy")] Data.ModelTree.empty
                  @?= Right (MNothing,
                             ModelTree MNothing (M.fromList [(T.pack "a", ModelTree MNothing (M.fromList [(T.pack "different", ModelTree MNothing (M.fromList [(T.pack "path", ModelTree (MInteger 42) M.empty)]))])),
                                                             (T.pack "some", ModelTree MNothing (M.fromList [(T.pack "path", ModelTree (MInteger 42) M.empty)]))]),
                             S.fromList [InsModel (toTreePath "some/path") (MInteger 42),
                                         Copy (toTreePath "some/path") (toTreePath "a/different/path")])

case_example_copy_miss = runAction "Example" [MText (T.pack "copy_miss")] Data.ModelTree.empty
                       @?= Right (MNothing, Data.ModelTree.empty,
                                  S.fromList [Copy (toTreePath "some/path") (toTreePath "a/different/path")])

case_example_read = runAction "Example" [MText (T.pack "read")] Data.ModelTree.empty
                       @?= Right (MInteger 23,
                                  ModelTree MNothing (M.fromList [(T.pack "some", ModelTree MNothing (M.fromList [(T.pack "path", ModelTree (MInteger 23) M.empty)]))]),
                                  S.fromList [InsModel (toTreePath "some/path") (MInteger 23)])

case_example_read_miss = runAction "Example" [MText (T.pack "read_miss")] Data.ModelTree.empty
                       @?= Right (MNothing, Data.ModelTree.empty, S.empty)

prop_example_error s = runAction "Example" [MText (T.pack ("error! " ++ s))] Data.ModelTree.empty == Left ("error! " ++ s)

testGroup = $(testGroupGenerator)
