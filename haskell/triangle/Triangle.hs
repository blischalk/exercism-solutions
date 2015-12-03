module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Illogical
                  | Scalene
                    deriving (Show, Eq)

triangleType :: Int -> Int -> Int -> TriangleType
triangleType a b c
    | illogical a b c                 = Illogical
    | a == b && b == c                = Equilateral
    | a == b || b == c || a == c      = Isosceles
    | a /= b && b /= c                = Scalene
    | otherwise                       = undefined
    where negativeSides d e f          = any (<= 0) [d,e,f]
          twoSmallerOrEqualThird d e f = d + e <= f || e + f <= d || d + f <= e
          illogical d e f              = negativeSides d e f
                                       || twoSmallerOrEqualThird d e f
