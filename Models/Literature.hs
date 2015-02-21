module Models.Literature where

import ClassyPrelude.Yesod

data LiteratureType = Book
                    | Paper
  deriving (Show, Read, Eq)

derivePersistField "LiteratureType"
