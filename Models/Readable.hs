module Models.Readable where

import Import hiding (on, (==.))
import Control.Lens hiding (from)
import Data.Aeson
import Database.Esqueleto hiding ((^.))
import qualified Database.Esqueleto as E

chartData :: [Record] -> Text
chartData records = decodeUtf8 $ toStrict $ encode $ map encoder records
  where
    encoder r = [r ^. recordPageStart, r ^. recordPageEnd]

userReadableReadings :: MonadIO m =>
                        UserId -> SqlPersistT m [(Entity Readable, Entity UserReading)]
userReadableReadings userId = select $ from $ \(r `LeftOuterJoin` u) -> do
             on (r E.^. ReadableId ==. u E.^. UserReadingReadableId)
             where_ (u E.^. UserReadingUserId ==. val userId)
             return (r, u)


maybeUserReadableReadings :: MonadIO m =>
                             UserId ->
                             SqlPersistT m [(Entity Readable, Maybe (Entity UserReading))]
maybeUserReadableReadings userId = select $ from $ \(r `LeftOuterJoin` mu) -> do
             on (just (r E.^. ReadableId) ==. mu ?. UserReadingReadableId)
             where_ (mu ?. UserReadingUserId ==. just (val userId))
             return (r, mu)
