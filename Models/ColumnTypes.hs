module Models.ColumnTypes where

import ClassyPrelude.Yesod

data ReadableType = Book
                    | Paper
                    deriving (Show, Read, Eq)

data ReadingStatus = Unread
                   | ToRead
                   | Reading
                   | DoneReading
                   deriving (Show, Read, Eq)

instance PathPiece ReadingStatus where
  fromPathPiece t | t == "unread" = Just Unread
                  | t == "to-read" = Just ToRead
                  | t == "reading" = Just Reading
                  | t == "done-reading" = Just DoneReading
                  | otherwise = Nothing

  toPathPiece Unread = "unread"
  toPathPiece ToRead = "to-read"
  toPathPiece Reading = "reading"
  toPathPiece DoneReading = "done-reading"

derivePersistField "ReadableType"
derivePersistField "ReadingStatus"
