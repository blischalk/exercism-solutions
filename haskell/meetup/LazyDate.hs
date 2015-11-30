module LazyDate where
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate

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

inYear :: Integer -> [Day] -> [Day]
inYear y dates = filter (\x -> let (dy,_) = toOrdinalDate x in dy == y) dates
