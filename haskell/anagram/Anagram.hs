module Anagram (anagramsFor) where

import Data.List
import Data.Char

anagramsFor :: String -> [String] -> [String]
anagramsFor _ [] = []
anagramsFor input (x:xs) = if a /= b && sort a == sort b
                           then x : anagramsFor a xs
                           else anagramsFor a xs
                               where a = map toLower input
                                     b = map toLower x
