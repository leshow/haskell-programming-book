{-# LANGUAGE DeriveFunctor #-}

module Main where

import           Ciphers            (unvigenere, vigenere)
import           Data.Bifunctor
import           System.Environment (getArgs)
import           System.Exit
import           System.IO

main :: IO ()
main = do
    args <- getArgs
    action <- parse args

    pure ()

parse :: [String] -> IO (Action Vigenere)
parse ["-e", file] = pure (Encrypt $ vEnc file)
parse ["-d", file] = pure (Decrypt $ vDec file)
parse _            = showUsage >> failExit

showUsage :: IO ()
showUsage = putStrLn "Usage: foo [-ed] file"

exit :: IO a
exit = exitSuccess

failExit :: IO a
failExit = exitWith (ExitFailure 1)

vEnc :: FilePath -> Vigenere
vEnc f = Vigenere { path = f, mode = ReadWriteMode }

vDec :: FilePath -> Vigenere
vDec f = Vigenere { path = f, mode = ReadWriteMode }

data Action a
    = Encrypt a
    | Decrypt a
    deriving (Show, Eq, Functor)

data Vigenere = Vigenere
    { path :: FilePath
    , mode :: IOMode
    } deriving (Show, Eq)
