module Models.Readable where

import Import
import Data.Aeson
import Control.Lens

chartData :: [Record] -> Text
chartData records = decodeUtf8 $ toStrict $ encode $ map encoder records
  where
    encoder r = [r ^. recordPageStart, r ^. recordPageEnd]
