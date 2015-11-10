module School (empty, add, sorted, grade, School) where

import Data.List
import qualified Data.Map as Map

type Grade      = Int
type Name       = String
type Classmates = [Name]
type School     = Map.Map Grade Classmates

empty :: School
empty = Map.empty

add :: Grade -> Name -> School -> School
add sg sn sch = Map.insertWith (++) sg [sn] sch

sorted :: School -> [(Grade,Classmates)]
sorted s = map (\(g,ns) -> (g, sort ns)) $ sort $ Map.toList s

grade :: Grade -> School -> Classmates
grade g s = case Map.lookup g s of
              Just cm -> cm
              Nothing -> []
