{-# LANGUAGE TemplateHaskell #-}
module Tests.ActionTests(testGroup) where

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck()

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Sequence

import Action
import Model
import Data.ModelTree

{-# ANN module "HLint: ignore Use camelCase" #-}

case_example_1 = runAction "Example" [] Data.ModelTree.empty @?= Right (MNothing, ModelTree MNothing (M.fromList [(T.pack "some", ModelTree MNothing (M.fromList [(T.pack "path", ModelTree (MInteger 42) M.empty)]))]), Data.Sequence.fromList [InsModel (toTreePath "some/path") (MInteger 42)])

testGroup = $(testGroupGenerator)
