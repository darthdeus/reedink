module Handler.Home where

import Import hiding (on, (==.))
import Database.Esqueleto
import qualified Database.Esqueleto as E

getProfileR :: Handler Html
getProfileR = do
  userId <- requireAuthId

  items <- runDB $ select $ from $ \(r `LeftOuterJoin` u) -> do
             on (r ^. ReadableId ==. u ^. UserReadingReadableId)
             where_ (u ^. UserReadingUserId ==. val userId)

  defaultLayout $(widgetFile "profile")
