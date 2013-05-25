{-# LANGUAGE TemplateHaskell #-}
module Tests.Model(testGroup) where

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck()


import Model
import qualified Model.Example

import Data.Serialize
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Sequence as S


{-# ANN module "HLint: ignore Use camelCase" #-}

recode x = decode (encode (x :: Model))
recode_eq x = recode x == Right x
recode_ x = recode x @?= Right x

arbilist1 t x = t (map (\y -> MInteger y) (x :: [Integer]))

prop_serial_integer x = recode (MInteger x) == Right (MInteger x)
prop_serial_rational x = recode (MRational x) == Right (MRational x)
prop_serial_list x = let v = (arbilist1 MList x); in recode v == Right v
prop_serial_seq x = let v = (arbilist1 (\y -> MSeq (S.fromList y)) x); in recode v == Right v
prop_serial_bs x = recode (MByteString x) == Right (MByteString x)
prop_serial_text x = recode (MText x) == Right (MText x)
prop_serial_example x = recode_eq (ModelExample x)

case_serial_Integer_0 = recode_ (MInteger 0)
case_serial_Integer_50 = recode_ (MInteger 50)
case_serial_Integer_n5 = recode_ (MInteger (-5))

case_serial_Text_0 = recode_ (MText (T.pack ""))
case_serial_Text_asdf = recode_ (MText (T.pack "asdf"))

case_serial_ByteString_0 = recode_ (MByteString (B.pack []))
case_serial_ByteString_1 = recode_ (MByteString (B.pack [1,2,3,4]))

case_serial_nuthin = recode_ MNothing

case_serial_Example_0 = recode_ (ModelExample (Model.Example.Example ""))
case_serial_Example_asdf = recode_ (ModelExample (Model.Example.Example "asdf"))

case_serial_bad_model = decode (B.pack [77,0,0,0,0,0,0,0,0]) @?= Right MNothing
case_serial_bad_subtype = decode (B.pack [217]) @?= Right MNothing

testGroup = $(testGroupGenerator)
