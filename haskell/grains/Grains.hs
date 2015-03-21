module Grains
( square
, total ) where

square :: Integer -> Integer
square sq = squareIter 1 sq 1

squareIter :: Integer -> Integer -> Integer -> Integer
squareIter a b c
  | a == b = c
  | otherwise = squareIter (a + 1) b (c * 2)

totalIter :: Integer -> Integer -> Integer -> Integer -> Integer
totalIter a b c d
  | a == b = d
  | otherwise = let a' = a + 1
                    c' = c * 2
                    d' = d + c'
                in totalIter a' b c' d'

total :: Integer
total = totalIter 1 64 1 1
