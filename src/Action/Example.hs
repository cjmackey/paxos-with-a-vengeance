module Action.Example where

import Model
import Control.Monad.Act

import qualified Data.Text as T

act :: [Model] -> Act (Either String Model)
act [MText t] = optest (T.unpack t)
act _ = do
  return (Right (MInteger 42))

optest "write" = do
  write "some/path" (MInteger 42)
  return (Right MNothing)

optest "copy" = do
  write "some/path" (MInteger 42)
  copy "some/path" "a/different/path"
  return (Right MNothing)

optest "copy_miss" = do
  copy "some/path" "a/different/path"
  return (Right MNothing)

optest errorMsg = do
  return (Left errorMsg)
