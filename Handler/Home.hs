module Handler.Home where

import Import hiding (on)
import Control.Lens
import Helpers.Common
import Models.Readable

getProfileR :: Handler Html
getProfileR = do
  userId <- requireAuthId
  items <- runDB $ userReadableReadings userId
  entries <- runDB $ selectList [EntryUserId ==. userId] []

  defaultLayout $(widgetFile "profile")
