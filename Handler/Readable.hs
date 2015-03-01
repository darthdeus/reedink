module Handler.Readable where

import Control.Lens
import Data.Maybe
import Import
import Yesod.Form.Bootstrap3
import qualified Data.Text as T

import Helpers.Common
import Models.ColumnTypes
import Models.Readable
import Models.Record

readableForm :: UserId -> Maybe Readable -> Form Readable
readableForm ownerId mr = renderBootstrap3 BootstrapBasicForm $ Readable
        <$> areq textField (bs "Title") (_readableTitle <$> mr)
        <*> aopt textField (bs "Description") (mr ^? _Just.readableDescription)
        <*> areq textField (bs "Author") (_readableAuthor <$> mr)
        <*> areq (selectFieldList types) (bs "Literature type") (_readableType <$> mr)
        <*> areq intField (bs "# pages") (_readablePageCount <$> mr)
        <*> aopt textField (bs "URL") (_readableUrl <$> mr)
        <*> pure (maybe ownerId id (_readableOwnerId <$> mr))
        <*  bootstrapSubmit (BootstrapSubmit ("Submit" :: Text) "btn-default" [])
        where
          types = map (\x -> (T.pack $ show x, x)) [Book, Paper]


getReadablesR :: Handler Html
getReadablesR = do
  items <- runDB $ selectList [] []
  defaultLayout $(widgetFile "readables")

getReadablesNewR :: Handler Html
getReadablesNewR = do
  userid <- requireAuthId
  (form, _) <- generateFormPost $ readableForm userid Nothing

  defaultLayout $(widgetFile "readables-new")

postReadablesR :: Handler Html
postReadablesR = do
  userid <- requireAuthId
  ((res, form), _) <- runFormPost $ readableForm userid Nothing

  case res of
    FormSuccess readable -> do
      void . runDB $ insert readable
      setMessage "Readable created."
      redirect ReadablesR

    _ -> defaultLayout $(widgetFile "readables-new")

getReadableR :: ReadableId -> Handler Html
getReadableR key = do
  userid <- requireAuthId
  readable <- runDB $ get404 key
  records <- runDB $ selectList [RecordReadableId ==. key,
                                 RecordUserId ==. userid] []

  mreading <- runDB $ selectFirst [UserReadingUserId ==. userid,
                                   UserReadingReadableId ==. key] []

  (form, _) <- generateFormPost $ recordForm key userid Nothing

  defaultLayout $(widgetFile "readable")

getReadableEditR :: ReadableId -> Handler Html
getReadableEditR key = do
  userId <- requireAuthId
  readable <- runDB $ get404 key

  when (readable ^. readableOwnerId /= userId) $ do
    setMessage "You're not authorized to edit this literature"
    redirect $ ReadableR key

  (form, _) <- generateFormPost $ readableForm userId $ Just readable

  defaultLayout $(widgetFile "readable-edit")

putReadableR :: ReadableId -> Handler Html
putReadableR key = do
  userId <- requireAuthId
  readable <- runDB $ get404 key

  when (readable ^. readableOwnerId /= userId) $ do
    setMessage "You're not authorized to edit this literature"
    redirect $ ReadableR key

  ((res,form), _) <- runFormPost $ readableForm userId $ Just readable

  case res of
    FormSuccess newReadable -> do
      void . runDB $ replace key newReadable
      setMessage "Update successful."
      redirect $ ReadableR key

    _ -> defaultLayout $(widgetFile "readable-edit")

deleteReadableR :: ReadableId -> Handler Html
deleteReadableR key = do
  userId <- requireAuthId
  readable <- runDB $ get404 key

  when (readable ^. readableOwnerId /= userId) $ do
    setMessage "You're not authorized to do this."
    redirect ReadablesR

  runDB $ do
    deleteWhere [RecordReadableId ==. key]
    deleteWhere [UserReadingReadableId ==. key]
    delete key

  setMessage "Deleted successful."
  redirect ReadablesR

postReadableStatusUpdateR :: ReadableId -> ReadingStatus -> Handler Html
postReadableStatusUpdateR key status = do
  userId <- requireAuthId
  mreading <- runDB $ selectFirst [UserReadingUserId ==. userId,
                                   UserReadingReadableId ==. key] []

  case mreading of
    Nothing -> void . runDB $ insert $ UserReading key userId status
    Just (Entity readingKey _) -> runDB $ update readingKey [UserReadingStatus =. status]

  setMessage "Status updated"
  redirect $ ReadableR key
