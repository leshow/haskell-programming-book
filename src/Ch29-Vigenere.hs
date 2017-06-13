{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import           Ciphers            (unvigenere, vigenere)
import           System.Environment (getArgs)
import qualified Data.ByteString as BS
import           System.Exit
import           System.IO

data Action
    = Encrypt
    | Decrypt
    deriving (Show, Eq)

data Vigenere = Vigenere
    { actionType :: Action
    , key        :: String
    } deriving (Show, Eq)

main :: IO ()
main = do
    args <- getArgs :: IO [String]
    Vigenere{..} <- parse args
    case actionType of
        Encrypt -> do
            input <- hGetContents stdin 
            timeUp stdin $ do 
                putStrLn "-------------"
                putStrLn "ENCRYPTED CONTENTS: "
                print $ vigenere key input
        Decrypt -> do
            input <- hGetContents stdin
            timeUp stdin $ do 
                putStrLn "-------------"
                putStrLn "DECRYPTED CONTENTS:"
                print $ unvigenere key input
    pure ()

timeUp :: Handle -> IO () -> IO ()
timeUp h x = do 
    bool <- hWaitForInput h 15
    if bool == True then x else putStrLn "Timeout. (15s)"

parse :: [String] -> IO Vigenere
parse ["-e", "-k", key] = print key >> pure Vigenere { key, actionType = Encrypt }
parse ["-ek", key] = print key >> pure Vigenere { key, actionType = Encrypt }
parse ["-d", "-k", key] = print key >> pure Vigenere { key, actionType = Decrypt }
parse ["-dk", key] = print key >> pure Vigenere { key, actionType = Decrypt }
parse _            = showUsage >> failExit

showUsage :: IO ()
showUsage = putStrLn "Usage: foo [-ed] file [-k] stringkey"

exit :: IO a
exit = exitSuccess

failExit :: IO a
failExit = exitWith $ ExitFailure 1