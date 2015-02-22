module Models.Readable where

import ClassyPrelude.Yesod

data ReadableType = Book
                    | Paper
  deriving (Show, Read, Eq)

derivePersistField "ReadableType"
