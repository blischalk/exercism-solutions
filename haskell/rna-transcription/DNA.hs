module DNA (toRNA) where

toRNA :: [Char] -> [Char]
toRNA [] = []
toRNA (x:xs) = case x of 'G' -> 'C'
                         'C' -> 'G'
                         'T' -> 'A'
                         'A' -> 'U'
                         _   -> error "undefined"
               : toRNA xs