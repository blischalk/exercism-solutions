module DNA (hammingDistance) where

hammingDistance :: String -> String -> Int
hammingDistance [] []         = 0
hammingDistance [] _          = 0
hammingDistance _  []         = 0
hammingDistance (x:xs) (y:ys) = (if x == y then 0 else 1) + hammingDistance xs ys
