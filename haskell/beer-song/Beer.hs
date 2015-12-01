module Beer (verse, sing) where

type Verse         = String
type Song          = String
type StartingVerse = Int
type EndingVerse   = Int

verse :: Int -> Verse
verse 2 = "2 bottles of beer on the wall, 2 bottles of beer.\nTake one down and pass it around, 1 bottle of beer on the wall.\n"
verse 1 = "1 bottle of beer on the wall, 1 bottle of beer.\nTake it down and pass it around, no more bottles of beer on the wall.\n"
verse 0 = "No more bottles of beer on the wall, no more bottles of beer.\nGo to the store and buy some more, 99 bottles of beer on the wall.\n"
verse n = let n'  = show n
              n'' = show $ n - 1
          in n' ++ " bottles of beer on the wall, " ++ n' ++ " bottles of beer.\nTake one down and pass it around, " ++ n'' ++ " bottles of beer on the wall.\n"

sing :: StartingVerse -> EndingVerse -> Song
sing a b = if a == b
           then verse a ++ "\n"
           else verse a ++ "\n" ++ (sing (a - 1) b)
