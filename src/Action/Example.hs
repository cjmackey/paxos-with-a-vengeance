module Action.Example where

import Model
import Control.Monad.Act

act :: [Model] -> Act (Either String Model)
act args = do
  write "some/path" (MInteger 42)
  return (Right MNothing)

