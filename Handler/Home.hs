module Handler.Home where

import Import hiding (on)
import Control.Lens
import Helpers.Common
import Models.Readable
import Yesod.Form.Bootstrap3

skillForm :: UserId -> Maybe Skill -> Form Skill
skillForm userId ms = renderBootstrap3 BootstrapBasicForm $ Skill
                    <$> areq textField (bs "Name") (_skillName <$> ms)
                    <*> pure (maybe userId id (_skillUserId <$> ms))
                    <* bootstrapSubmit (BootstrapSubmit ("Submit" :: Text) "btn-default" [])

getProfileR :: Handler Html
getProfileR = do
  userId <- requireAuthId
  items <- runDB $ userReadableReadings userId
  entries <- runDB $ selectList [EntryUserId ==. userId] []

  defaultLayout $(widgetFile "profile")

getSkillsR :: Handler Html
getSkillsR = do
  userId <- requireAuthId
  skills <- runDB $ selectList [SkillUserId ==. userId] []

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
  runDB $ delete key
  setMessage "Skill deleted"
  redirect SkillsR

postProgressR :: SkillId -> Handler Html
postProgressR key = do
  userId <- requireAuthId
  day <- fmap utctDay $ liftIO getCurrentTime
  mprogress <- runDB $ selectFirst [ProgressSkillId ==. key,
                                    ProgressUserId ==. userId,
                                    ProgressCreatedAt ==. day] []

  case mprogress of
    Just _ ->
      setMessage "You've already marked progress on this skill today"

    Nothing -> do
      void . runDB $ insert $ Progress key day userId
      setMessage "Progress marked"

  redirect SkillsR
