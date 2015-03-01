module Handler.Home where

import Control.Lens hiding (from, on)
import Import hiding (on, (==.))
import Database.Esqueleto hiding ((^.))
import qualified Database.Esqueleto as E

getProfileR :: Handler Html
getProfileR = do
  userId <- requireAuthId

  items <- runDB $ select $ from $ \(r `LeftOuterJoin` u) -> do
             on (r E.^. ReadableId ==. u E.^. UserReadingReadableId)
             where_ (u E.^. UserReadingUserId ==. val userId)
             return (r, u)

  defaultLayout $(widgetFile "profile")
