module LeapYear (isLeapYear) where

isLeapYear :: Int -> Bool
isLeapYear y 
  | y `rem` 4 == 0 = if y `rem` 100 == 0 && y `rem` 400 /= 0
                     then False
                     else True
  | otherwise      = False