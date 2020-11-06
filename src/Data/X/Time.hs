module Data.X.Time where

import Data.Time (UTCTime (UTCTime), addUTCTime, nominalDay)

subtractDay :: Int -> UTCTime -> UTCTime
subtractDay 0 time = time
subtractDay n time = addUTCTime diff time
  where
    diff = nominalDay * realToFrac (- n)

startOfDay :: UTCTime -> UTCTime
startOfDay (UTCTime d _) = UTCTime d 0

endOfDay :: UTCTime -> UTCTime
endOfDay (UTCTime d _) = UTCTime d 86400
