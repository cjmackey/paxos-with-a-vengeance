{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Framework (defaultMain)
import Test.Framework.TH

import Data.ModelTree()
import Data.NodeState()
import Model()

import qualified Tests.Model

{-# ANN module "HLint: ignore Use camelCase" #-}

main :: IO ()
main = defaultMain [ $(testGroupGenerator)
                   , Tests.Model.testGroup
                   ]
