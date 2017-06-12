
module Main where

import           Ciphers            (unvigenere, vigenere)
import           Data.Bifunctor
import           System.Environment (getArgs)
import           System.Exit
import           System.IO

data Action
    = Encrypt
    | Decrypt
    deriving (Show, Eq)

data Vigenere = Vigenere
    { path   :: FilePath
    , mode   :: IOMode
    , action :: Action
    } deriving (Show, Eq)


vEnc :: FilePath -> Vigenere
vEnc f = Vigenere { path = f, mode = ReadWriteMode, action = Encrypt  }

vDec :: FilePath -> Vigenere
vDec f = Vigenere { path = f, mode = ReadWriteMode, action = Decrypt }

main :: IO ()
main = do
    args <- getArgs
    action <- parse args
    handle <- openFile (path action) (mode action)
    pure ()

parse :: [String] -> IO Vigenere
parse ["-e", file] = pure $ vEnc file
parse ["-d", file] = pure $ vDec file
parse _            = showUsage >> failExit

showUsage :: IO ()
showUsage = putStrLn "Usage: foo [-ed] file"

exit :: IO a
exit = exitSuccess

failExit :: IO a
failExit = exitWith (ExitFailure 1)


