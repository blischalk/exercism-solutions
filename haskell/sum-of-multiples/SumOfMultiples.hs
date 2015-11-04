module SumOfMultiples (sumOfMultiples,sumOfMultiplesDefault) where

isMultiple :: [Int] -> Int -> Bool
isMultiple ms i = any (\x -> x == True) $ map (\x -> i `mod` x == 0) ms

sumOfMultiplesDefault :: Int -> Int
sumOfMultiplesDefault i = foldl (+) 0 [x | x <- [1..i-1], isMultiple [3,5] x]

sumOfMultiples :: [Int] -> Int -> Int
sumOfMultiples ms i = foldl (+) 0 [x | x <- [1..i-1], isMultiple ms x]
