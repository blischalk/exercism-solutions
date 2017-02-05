module LeapYear (isLeapYear) where

isLeapYear :: Int -> Bool
isLeapYear y = y `rem` 4 == 0 &&
               (not $ y `rem` 100 == 0 && y `rem` 400 /= 0)
