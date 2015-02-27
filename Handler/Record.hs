module Handler.Record where

import Import
import Models.Record

postRecordsR :: ReadableId -> Handler Html
postRecordsR readableId = do
  userId <- requireAuthId
  ((res, _), _) <- runFormPost $ recordForm readableId userId Nothing

  case res of
    FormSuccess record -> do
      void . runDB $ insert record
      setMessage "Reading record saved"

    _ -> setMessage "Invalid data"

  redirect $ ReadableR readableId
