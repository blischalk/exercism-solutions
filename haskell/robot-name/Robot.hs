module Robot (mkRobot, robotName, resetName) where
import System.Random (randomRIO)
import Data.IORef (IORef, newIORef, modifyIORef, readIORef)
import Control.Monad (replicateM)

data Robot = Robot {name :: String} deriving Show

numbers :: (Char,Char)
numbers = ('0','9')

letters :: (Char,Char)
letters = ('A','Z')

mkRobot :: IO (IORef Robot)
mkRobot = genName >>= newIORef . Robot

robotName :: IORef Robot -> IO String
robotName r = readIORef r >>= return . name

resetName :: IORef Robot -> IO ()
resetName r = genName >>= (\newName -> modifyIORef r
                                       (\rbt -> rbt {name=newName}))

genName :: IO String
genName = replicateM 2 getLetter >>=
          \l -> replicateM 3 getNumber >>=
                \n -> return $ l ++ n

getLetter :: IO Char
getLetter = randomRIO letters

getNumber :: IO Char
getNumber = randomRIO numbers
