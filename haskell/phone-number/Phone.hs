module Phone (number,areaCode,prettyPrint) where
import Data.Char (isDigit)

type WrittenPhoneNumber = String
type ParsedPhoneNumber = String
type AreaCode = String
type PrettyPhoneNumber = String


number :: WrittenPhoneNumber -> ParsedPhoneNumber
number input
    | len == 10 = digits
    | len == 11 && head digits == '1' = tail digits
    | otherwise = replicate 10 '0'
    where digits = filter isDigit input
          len = length digits


areaCode :: WrittenPhoneNumber -> AreaCode
areaCode input = take 3 input


prettyPrint :: ParsedPhoneNumber -> PrettyPhoneNumber
prettyPrint input = let parsed = number input
                        f = areaCode parsed
                        s = take 3 $ drop 3 parsed
                        t = take 4 $drop 3 $ drop 3 parsed
                    in "(" ++ f ++ ")" ++ " " ++ s ++ "-" ++ t
