module DNA (toRNA) where

toRNA :: [Char] -> [Char]
toRNA xs = 
  foldr (\x y ->
      let v = case x of 'G' -> 'C'
                        'C' -> 'G'
                        'T' -> 'A'
                        'A' -> 'U'
                        _   -> error $ "Undefined transformation encountered for char: " ++ show x
      in v:y)
  []
  xs
