module Handler.Home where

import Import hiding (on, (==.))
import qualified Import as I
import Helpers.Common
import Models.Readable
import Yesod.Form.Bootstrap3
import Database.Esqueleto
import qualified Database.Esqueleto as E

skillForm :: UserId -> Maybe Skill -> Form Skill
skillForm userId ms = renderBootstrap3 BootstrapBasicForm $ Skill
                    <$> areq textField (bs "Name") (_skillName <$> ms)
                    <*> pure (maybe userId id (_skillUserId <$> ms))
                    <* bootstrapSubmit (BootstrapSubmit ("Submit" :: Text) "btn-default" [])

getProfileR :: Handler Html
getProfileR = do
  userId <- requireAuthId
  items <- runDB $ userReadableReadings userId
  entries <- runDB $ selectList [EntryUserId I.==. userId] []

  defaultLayout $(widgetFile "profile")

getSkillsR :: Handler Html
getSkillsR = do
  userId <- requireAuthId
  day <- fmap utctDay $ liftIO getCurrentTime

  items <- runDB $ select $ from $ \(s `LeftOuterJoin` mp) -> do
    on ((just (s E.^. SkillId) ==. mp ?. ProgressSkillId) &&.
        (mp ?. ProgressCreatedAt ==. just (val day)))
    where_ (s E.^. SkillUserId ==. val userId)
    return (s, mp)

  (form, _) <- generateFormPost $ skillForm userId Nothing

  defaultLayout $(widgetFile "skills")

postSkillsR :: Handler Html
postSkillsR = do
  userId <- requireAuthId
  ((res, _), _) <- runFormPost $ skillForm userId Nothing

  case res of
    FormSuccess skill -> do
      void . runDB $ insert skill
      setMessage "Skill created"
      redirect SkillsR

    _ -> do
      setMessage "Failed to create a skill"
      redirect SkillsR

deleteSkillR :: SkillId -> Handler Html
deleteSkillR key = do
  -- TODO - only delete skills that belong to the currently logged in user
  -- TODO - delete all Progress that belongs to a given skill
  runDB $ I.delete key
  setMessage "Skill deleted"
  redirect SkillsR

postProgressR :: SkillId -> Handler Html
postProgressR key = do
  userId <- requireAuthId

  day <- fmap utctDay $ liftIO getCurrentTime
  mprogress <- runDB $ selectFirst [ProgressSkillId I.==. key,
                                    ProgressUserId I.==. userId,
                                    ProgressCreatedAt I.==. day] []

  case mprogress of
    Just _ ->
      setMessage "You've already marked progress on this skill today"

    Nothing -> do
      void . runDB $ insert $ Progress key day userId
      setMessage "Progress marked"

  redirect SkillsR
