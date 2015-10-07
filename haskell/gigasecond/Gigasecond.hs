module Gigasecond (fromDay) where
import Data.Time.Clock (UTCTime, addUTCTime)

fromDay :: UTCTime -> UTCTime
fromDay t = 1000000000 `addUTCTime` t
