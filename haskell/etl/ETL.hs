module ETL (transform) where

import qualified Data.Map.Strict as Map
import Data.Char

type Letter = String
type Points = Int

flattenOldMap :: [(Points, [Letter])] -> [(Letter, Points)]
flattenOldMap l = Prelude.foldl (++) [] $ Prelude.map (\(point,letters) -> (Prelude.map (\ch -> (ch,point)) letters)) l

transform :: Map.Map Points [Letter] -> Map.Map Letter Points
transform oldData = let oldDataMapAsList      = Map.toList oldData
                        formattedForInsertion = flattenOldMap oldDataMapAsList
                    in Map.fromList $ Prelude.map toLowerString formattedForInsertion

toLowerString :: (Letter, Points) -> (Letter, Points)
toLowerString (c,p) = (map toLower c,p)
