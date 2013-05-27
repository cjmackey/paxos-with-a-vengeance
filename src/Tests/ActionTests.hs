{-# LANGUAGE TemplateHaskell #-}
module Tests.ActionTests(testGroup) where

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck()

import qualified Data.Map as M
import qualified Data.Text as T


import Action
import Model
import Data.ModelTree

{-# ANN module "HLint: ignore Use camelCase" #-}

case_example_1 = actDirectly "Example" [] Data.ModelTree.empty @?= Right (ModelTree MNothing (M.fromList [(T.pack "some", ModelTree MNothing (M.fromList [(T.pack "path", ModelTree (MInteger 42) M.empty)]))]))

testGroup = $(testGroupGenerator)
