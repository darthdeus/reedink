module Models.Readable where

import ClassyPrelude.Yesod
import Models.ColumnTypes

derivePersistField "ReadableType"
derivePersistField "ReadingStatus"
