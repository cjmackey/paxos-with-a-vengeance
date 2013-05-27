{-# LANGUAGE TemplateHaskell #-}
module Tests.Data.ModelTreeTests(testGroup) where

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck()

import Prelude hiding (lookup)
import Data.Serialize
import qualified Data.Map as M
import qualified Data.Text as T

import Model
import Data.ModelTree

{-# ANN module "HLint: ignore Use camelCase" #-}

recode x = decode (encode (x :: ModelTree))
recode_eq x = recode x == Right x
recode_ x = recode x @?= Right x

complexModelTree = (ModelTree (MText $ T.pack "lol") (M.fromList [(T.pack "asdf", ModelTree (MInteger 17) M.empty)]))

case_serial_simple = recode_ (ModelTree MNothing M.empty)
case_serial_complex = recode_ complexModelTree

case_tree_path_string = toTreePath "/asdf/blah/hello" @?= map T.pack ["asdf","blah","hello"]
case_tree_path_string2 = toTreePath "asdf/blah/hello" @?= map T.pack ["asdf","blah","hello"]

case_applyTreeMod_0 = applyTreeMod (treeMod "" (MInteger 5)) empty @?= ModelTree (MInteger 5) M.empty

case_lookup_0 = lookup "" empty @?= Just empty
case_lookup_1 = lookup "asdf" empty @?= Nothing
case_lookup_1_1 = lookup "" complexModelTree @?= Just complexModelTree
case_lookup_1_1' = lookup' "" complexModelTree @?= (MText $ T.pack "lol")
case_lookup_2 = lookup "asdf" complexModelTree @?= Just (ModelTree (MInteger 17) M.empty)
case_lookup_3 = lookup "asdf/blah3" complexModelTree @?= Nothing

testGroup = $(testGroupGenerator)
