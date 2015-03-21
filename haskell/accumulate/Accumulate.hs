module Accumulate (accumulate) where

-- We need to use a and b here because
-- we don't know what the return type
-- of the passed in mapping function
-- will return.
accumulate :: (a -> b) -> [a] -> [b]
accumulate _ [] = []
accumulate f (x:xs) = f x : accumulate f xs