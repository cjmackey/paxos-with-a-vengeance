module Action.Example where

import Prelude hiding (read)

import Model
import Control.Monad.Act

import qualified Data.Text as T

import Control.Monad

act :: [Model] -> Act (Either String Model)
act [MText t] = optest (T.unpack t)
act _ = return (Right (MInteger 42))

optest "write" = do
  write "some/path" (MInteger 42)
  return (Right MNothing)

optest "manywrite" = do
  forM_ [1..10000] (\i -> write (show i) (MInteger i))
  x <- read "7777"
  return (Right x)

optest "read" = do
  write "some/path" (MInteger 23)
  x <- read "some/path"
  return (Right x)

optest "read_miss" = do
  x <- read "some/path"
  return (Right x)

optest "copy" = do
  write "some/path" (MInteger 42)
  copy "some/path" "a/different/path"
  return (Right MNothing)

optest "copy_miss" = do
  copy "some/path" "a/different/path"
  return (Right MNothing)

optest errorMsg = return (Left errorMsg)
