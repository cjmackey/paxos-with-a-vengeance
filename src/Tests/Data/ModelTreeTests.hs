{-# LANGUAGE TemplateHaskell #-}
module Tests.Data.ModelTreeTests(testGroup) where

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck()

import Data.Serialize
import qualified Data.Map as M
import qualified Data.Text as T

import Model
import Data.ModelTree

{-# ANN module "HLint: ignore Use camelCase" #-}

recode x = decode (encode (x :: ModelTree))
recode_eq x = recode x == Right x
recode_ x = recode x @?= Right x

case_serial_simple = recode_ (ModelTree MNothing M.empty)
case_serial_complex = recode_ (ModelTree (MText $ T.pack "lol") (M.fromList [(T.pack "asdf", ModelTree (MInteger 17) M.empty)]))


testGroup = $(testGroupGenerator)
