module Models.Record where

import Import
import Yesod.Form.Bootstrap3
import Helpers.Common

recordPagesTotal :: Record -> Int
recordPagesTotal r = _recordPageEnd r - _recordPageStart r

recordForm :: ReadableId -> UserId -> Maybe Record -> Form Record
recordForm readableId userId mrecord = renderBootstrap3 BootstrapBasicForm $
  Record <$> pure readableId
         <*> areq intField (bs "Start page") (_recordPageStart <$> mrecord)
         <*> areq intField (bs "End page") (_recordPageEnd <$> mrecord)
         <*> pure userId
         <*> lift createdAt
         <* submitButton "Submit"
  where
    createdAt =
      case mrecord of
        Nothing     -> (liftIO getCurrentTime)
        Just record -> return $ _recordCreatedAt record
