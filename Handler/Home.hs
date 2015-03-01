module Handler.Home where

import Import hiding (on, (==.))
import Control.Lens
import Models.Readable

getProfileR :: Handler Html
getProfileR = do
  userId <- requireAuthId
  items <- runDB $ userReadableReadings userId

  defaultLayout $(widgetFile "profile")
