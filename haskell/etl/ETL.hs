module ETL (transform) where

import qualified Data.Map.Strict as Map
import Data.Char

type Letter = String
type Points = Int

transform :: Map.Map Points [Letter] -> Map.Map Letter Points
transform oldData = let oldDataMapAsList      = Map.toList oldData
                        formattedForInsertion = flattenOldMap oldDataMapAsList
                    in Map.fromList $ Prelude.map toLowerString formattedForInsertion

flattenOldMap :: [(Points, [Letter])] -> [(Letter, Points)]
flattenOldMap l = Prelude.foldl (++) [] $ flattenLetters l

flattenLetters :: [(Points, [Letter])] -> [[(Letter, Points)]]
flattenLetters = Prelude.map (\(point,letters) ->
                                  (Prelude.map (\ch ->
                                                (ch,point)) letters))

toLowerString :: (Letter, Points) -> (Letter, Points)
toLowerString (c,p) = (map toLower c,p)
