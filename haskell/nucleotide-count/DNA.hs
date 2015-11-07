module DNA (count, nucleotideCounts) where

import qualified Data.Map as Mp
import qualified Data.Set as Set


validNucleotides :: [Char]
validNucleotides = "GATC"

charValid :: Char -> Bool
charValid c = if Set.member c $ Set.fromList validNucleotides
              then True
              else (error ("invalid nucleotide '" ++ [c] ++ "'"))


count :: Char -> [Char] -> Int
count c s = length $ filter (\x -> charValid c && charValid x && x == c) s


nucleotideCounts :: String -> Mp.Map Char Int
nucleotideCounts s = let els = Set.fromList validNucleotides
                     in Mp.fromList $ Set.toList (Set.map (\x -> (x, (count x s))) els)
