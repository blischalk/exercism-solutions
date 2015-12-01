module Meetup
( Weekday(..)
, Schedule(..)
, meetupDay
) where

import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDate

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday deriving (Enum,Show)

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth deriving (Enum,Show)

type Year = Integer
type Month = Int

inc :: Num a => a -> a
inc n = (+1) n

dateIter :: Integer -> [Day]
dateIter n = ModifiedJulianDay n : dateIter (inc n)

allDates :: [Day]
allDates = dateIter 0

ordinalDatePred :: (Integer -> t1 -> t) -> t1 -> Day -> t
ordinalDatePred f y x = let (dy,_) = toOrdinalDate x
                        in dy `f` y

inYear :: Integer -> [Day] -> [Day]
inYear y dates = takeWhile (ordinalDatePred (==) y) $
                 dropWhile (ordinalDatePred (/=) y) $
                 dates

inMonth :: Int -> [Day] -> [Day]
inMonth m dates = filter (\x -> let (_,gm,_) = toGregorian x
                                in gm == m) dates

isDayOfWeek :: Enum a => a -> [Day] -> [Day]
isDayOfWeek d dates = filter (\x -> let (_,_,cd) = toWeekDate x
                                    in (inc (fromEnum d)) == cd ) $
                      dates

scheduledOn :: Schedule -> [Day] -> [Day]
scheduledOn Teenth dates = filter (\x -> let (_,_,day) = toGregorian x
                                         in elem day [13..19]) dates
scheduledOn Last   dates = last dates : []
scheduledOn sch    dates = (dates !! fromEnum sch) : []

meetupDay :: Schedule -> Weekday -> Year -> Month -> Day
meetupDay schedule weekday year month = head $
                                        scheduledOn schedule $
                                        isDayOfWeek weekday $
                                        inMonth month $
                                        inYear year allDates
