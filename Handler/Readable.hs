module Handler.Readable where

import Control.Lens
import Data.Maybe
import Import
import Yesod.Form.Bootstrap3
import qualified Data.Text as T

import Helpers.Common
import Models.Readable
import Models.Record

readableForm :: UserId -> Maybe Readable -> Form Readable
readableForm ownerId mr = renderBootstrap3 BootstrapBasicForm $ Readable
        <$> areq textField (bs "Title") (_readableTitle <$> mr)
        <*> areq textField (bs "Description") (_readableDescription <$> mr)
        <*> areq (selectFieldList types) (bs "Literature type") (_readableType <$> mr)
        <*> areq textField (bs "Author") (_readableAuthor <$> mr)
        <*> areq intField (bs "# pages") (_readablePageCount <$> mr)
        <*> pure (maybe ownerId id (_readableOwnerId <$> mr))
        <*  bootstrapSubmit (BootstrapSubmit ("Submit" :: Text) "btn-default" [])
        where
          types = map (\x -> (T.pack $ show x, x)) [Book, Paper]


getReadablesR :: Handler Html
getReadablesR = do
  userid <- requireAuthId
  (form, _) <- generateFormPost $ readableForm userid Nothing

  items <- runDB $ selectList [] []
  defaultLayout $(widgetFile "readables")

postReadablesR :: Handler Html
postReadablesR = do
  userid <- requireAuthId
  ((res, form), _) <- runFormPost $ readableForm userid Nothing

  case res of
    FormSuccess readable -> do
      void . runDB $ insert readable
      setMessage "Readable created."
      redirect ReadablesR

    _ -> do
      items <- runDB $ selectList [] []
      defaultLayout $(widgetFile "readables")

getReadableR :: ReadableId -> Handler Html
getReadableR key = do
  userid <- requireAuthId
  readable <- runDB $ get404 key
  records <- runDB $ selectList [RecordReadableId ==. key] []

  (recform, _) <- generateFormPost $ recordForm key userid Nothing
  (form, _) <- generateFormPost $ readableForm userid $ Just readable

  defaultLayout $(widgetFile "readable")

putReadableR :: ReadableId -> Handler Html
putReadableR key = do
  userid <- requireAuthId
  readable <- runDB $ get404 key
  records <- runDB $ selectList [RecordReadableId ==. key] []

  (recform, _) <- generateFormPost $ recordForm key userid Nothing
  ((res,form), _) <- runFormPost $ readableForm userid $ Just readable

  case res of
    FormSuccess newReadable -> do
      void . runDB $ replace key newReadable
      setMessage "Update successful."
      redirect $ ReadableR key


    _ -> defaultLayout $(widgetFile "readable")

deleteReadableR :: ReadableId -> Handler Html
deleteReadableR key = do
  void . runDB $ delete key
  setMessage "Deleted successful."
  redirect ReadablesR
