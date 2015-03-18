module Models.Readable where

import Import hiding (on, (==.))
import Control.Monad.ST
import Data.STRef
import Data.Aeson
import Database.Esqueleto
import qualified Database.Esqueleto as E
import Debug.Trace

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

type ProgressToday = Maybe (Entity Progress)
type DailyProgress = (Day, Maybe (Entity Progress))

skillsWithProgress ::
  MonadIO m =>
  UserId
  -> Day
  -> SqlPersistT m [(Entity Skill, ProgressToday, [DailyProgress])]
skillsWithProgress userId day = do
  skills <- select $ from $ \(s `LeftOuterJoin` mp) -> do
              on ((just (s ^. SkillId) ==. mp ?. ProgressSkillId) &&.
                  (mp ?. ProgressCreatedAt ==. just (val day)))
              where_ (s ^. SkillUserId ==. val userId)
              return (s, mp)

  forM skills $ \(skill@(Entity key _), mp) -> do
    ps <- select $ from $ \p -> do
            where_ (p ^. ProgressSkillId ==. val key)
            orderBy [asc (p ^. ProgressCreatedAt)]
            return p

    let dailyProgresses =
          runST $ do
            ref <- newSTRef ps

            forM (take 2 $ [day,pred day ..]) $ \d -> do
              currentProgresses <- readSTRef ref

              case currentProgresses of
                [] -> do
                  return (d, Nothing)
                (p:rest) -> do
                  if _progressCreatedAt (entityVal p) == d
                    then do
                      writeSTRef ref rest
                      return (d, Just p)
                    else return (d, Nothing)

    return (skill, mp, dailyProgresses)
