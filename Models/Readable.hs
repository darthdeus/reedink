module Models.Readable where

import Import hiding (on, (==.))
import Data.Aeson
import Database.Esqueleto
import qualified Database.Esqueleto as E

chartData :: [Record] -> Text
chartData records = decodeUtf8 $ toStrict $ encode $ map encoder records
  where
    encoder r = [_recordPageStart r, _recordPageEnd r]

userReadableReadings :: MonadIO m =>
                        UserId -> SqlPersistT m [(Entity Readable, Entity UserReading)]
userReadableReadings userId =
  select $ from $ \(r `LeftOuterJoin` u) -> do
    on (r E.^. ReadableId ==. u E.^. UserReadingReadableId)
    where_ (u E.^. UserReadingUserId ==. val userId)
    return (r, u)


maybeUserReadableReadings :: MonadIO m =>
                             UserId ->
                             SqlPersistT m [(Entity Readable, Maybe (Entity UserReading))]
maybeUserReadableReadings userId =
  select $ from $ \(r `LeftOuterJoin` mu) -> do
    on ((just (r E.^. ReadableId) ==. mu ?. UserReadingReadableId) &&.
        (mu ?. UserReadingUserId ==. just (val userId)))
    return (r, mu)

-- skillProgression :: MonadIO m =>
--                     UserId ->
--                     SqlPersistT m [(Entity Skill, [Entity Progress])]
-- skillProgression userId = do

--   skills <- select $ from $ \s -> do
--     where_ (s ^. SkillUserId ==. val userId)
--     return s


skillsWithProgress ::
  MonadIO m =>
  UserId
  -> Day
  -> SqlPersistT m [(Entity Skill, Maybe (Entity Progress), [Entity Progress])]
skillsWithProgress userId day = do
  skills <- select $ from $ \(s `LeftOuterJoin` mp) -> do
    on ((just (s ^. SkillId) ==. mp ?. ProgressSkillId) &&.
        (mp ?. ProgressCreatedAt ==. just (val day)))
    where_ (s ^. SkillUserId ==. val userId)
    return (s, mp)

  forM skills $ \(skill@(Entity key _), mp) -> do
    ps <- select $ from $ \p -> do
            where_ (p ^. ProgressSkillId ==. val key)
            return p

    return (skill, mp, ps)
