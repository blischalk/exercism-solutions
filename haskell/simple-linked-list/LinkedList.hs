module LinkedList (isNil,
                   nil,
                   new,
                   next,
                   datum,
                   toList,
                   fromList,
                   reverseLinkedList)
where

data List a = Empty
            | Cons a (List a)
              deriving (Show, Read, Eq, Ord)

nil :: List a
nil = Empty

new :: a -> List a -> List a
new d Empty = Cons d Empty
new d l     = Cons d l

datum :: List t -> t
datum Empty      = undefined
datum (Cons t _) = t

isNil :: List t -> Bool
isNil Empty      = True
isNil (Cons _ _) = False

next :: List t -> List t
next Empty      = undefined
next (Cons _ n) = n

toList :: List t -> [t]
toList Empty      = []
toList (Cons t n) = t : (toList n)

fromList :: [t] -> List t
fromList []     = nil
fromList (x:xs) = new x (fromList xs)

reverseLinkedList :: List t -> List t
reverseLinkedList Empty = nil
reverseLinkedList l = fromList $ reverse $ toList l
