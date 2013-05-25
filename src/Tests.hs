{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Framework (defaultMain)
import Test.Framework.TH

import Model

--import Tests.CTypeTests
--import Tests.CClassTests
--import Tests.ParserTests

{-# ANN module "HLint: ignore Use camelCase" #-}

main :: IO ()
main = defaultMain [ $(testGroupGenerator)
--                   , Tests.CTypeTests.testGroup
 --                  , Tests.CClassTests.testGroup
  --                 , Tests.ParserTests.testGroup
                   ]
