module Models.Record where

import Import

recordPagesTotal :: Record -> Int
recordPagesTotal r = _recordPageStart r - _recordPageEnd r
