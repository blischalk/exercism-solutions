module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury | Venus | Earth | Mars | Jupiter | Saturn | Neptune | Uranus

earthYrInSecs :: Double
earthYrInSecs = 31557600

-- Returns time in planetary earth years
ageOn :: Planet -> Double -> Double
ageOn Mercury s = s / (0.2408467 * earthYrInSecs)
ageOn Venus   s = s / (0.61519726 * earthYrInSecs)
ageOn Earth   s = s / earthYrInSecs
ageOn Mars    s = s / (1.8808158 * earthYrInSecs)
ageOn Jupiter s = s / (11.862615 * earthYrInSecs)
ageOn Saturn  s = s / (29.447498 * earthYrInSecs)
ageOn Uranus  s = s / (84.016846 * earthYrInSecs)
ageOn Neptune s = s / (164.79132 * earthYrInSecs)
