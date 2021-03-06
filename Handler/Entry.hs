module Handler.Entry where

import Import
import Yesod.Form.Bootstrap3
import Helpers.Common
import Control.Lens

getEntriesR :: Handler Html
getEntriesR = do
  userId <- requireAuthId
  skills <- runDB $ selectList [SkillUserId ==. userId] []

  (form, _) <- generateFormPost $ entryForm skills userId Nothing
  entries <- runDB $ selectList [EntryUserId ==. userId] []
  defaultLayout $(widgetFile "entries")


postEntriesR :: Handler Html
postEntriesR = do
  userId <- requireAuthId
  ((res, form), _) <- runFormPost $ entryForm [] userId Nothing

  case res of
    FormSuccess entry -> do
      void . runDB $ insert entry
      setMessage "New knowledge has been logged."
      redirect EntriesR
    _ -> do
      setMessage "Something went wrong"
      redirect EntriesR

deleteEntryR :: EntryId -> Handler Html
deleteEntryR entryId = do
  userId <- requireAuthId
  entry <- runDB $ get404 entryId

  runDB $ do
    delete entryId

  setMessage "Deleted successful."
  redirect EntriesR

entryForm :: [Entity Skill] -> UserId -> Maybe Entry -> Form Entry
entryForm skills userId mentry = renderBootstrap3 BootstrapBasicForm $ Entry
  <$> areq textField (bs "Title") (mentry ^? _Just.entryTitle)
  <*> aopt textField (bs "Description") (mentry ^? _Just.entryDescription)
  <*> pure (maybe userId id (_entryUserId <$> mentry))
  <*> lift (liftIO getCurrentTime)
  <*> areq (selectFieldList skillList) "Skills" (mentry ^? _Just.entrySkillId)
  <*  bootstrapSubmit (BootstrapSubmit ("Submit" :: Text) "btn-default" [])
  where
    skillList = map (\(Entity key skill) -> (_skillName skill, key)) skills

