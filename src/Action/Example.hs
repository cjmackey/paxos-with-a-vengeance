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

optest errorMsg = do
  return (Left errorMsg)
