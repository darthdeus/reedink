module Models.ColumnTypes where

import ClassyPrelude.Yesod

data ReadableType = Book
                    | Paper
                    deriving (Show, Read, Eq)

data ReadingStatus = Unread
                   | Reading
                   | DoneReading
                   deriving (Show, Read, Eq)

instance PathPiece ReadingStatus where
  fromPathPiece t | t == "unread" = Just Unread
                  | t == "reading" = Just Reading
                  | t == "done-reading" = Just DoneReading
                  | otherwise = Nothing

  toPathPiece Unread = "unread"
  toPathPiece Reading = "reading"
  toPathPiece DoneReading = "done-reading"
