module Bob (responseFor) where
import Data.Char

isQuestion i = last i == '?'
isYelling i  = any isAlpha i && not (any isLower i)
isBlank i    = all isSpace i

responseFor i
  | isBlank i    = "Fine. Be that way!"
  | isYelling i  = "Whoa, chill out!"
  | isQuestion i = "Sure."
  | otherwise    = "Whatever."
