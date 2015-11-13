module WordCount (wordCount) where

import qualified Data.Map as Map
import Data.List.Split
import Data.Char

type Word  = String
type Count = Int

wordCount :: String -> Map.Map Word Count
wordCount s = countEm $ findWords s

countEm :: [String] -> Map.Map Word Count
countEm = foldl updateWordMapCounts Map.empty

updateWordMapCounts :: Map.Map Word Count -> Word -> Map.Map Word Count
updateWordMapCounts acc x = Map.insertWith (+) (strToLower x) 1 acc

strToLower :: String -> String
strToLower = map toLower

findWords :: String -> [String]
findWords s = filter notBlankStr $ splitWhen splitWorthy s
    where splitWorthy c = (and [((not . isAlpha) c), ((not . isNumber) c)])

notBlankStr :: String -> Bool
notBlankStr = not . (== "")
