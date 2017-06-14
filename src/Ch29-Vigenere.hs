{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ExplicitForAll #-}

module Main where

import           Ciphers            (unvigenere, vigenere)
import           System.Environment (getArgs)
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
    args <- getArgs 
    v <- parse args
    hasInput <- hWaitForInput stdin 15000 
    if hasInput 
        then do
            input <- getContents
            handleAction v input
        else hPutStrLn stderr "Timeout. (15s)"

handleAction :: Vigenere -> String -> IO ()
handleAction Vigenere{..} input =
    case actionType of
        Encrypt -> do
            putStrLn "-------------"
            putStrLn "ENCRYPTED CONTENTS: "
            print $ vigenere key input
        Decrypt -> do 
            putStrLn "-------------"
            putStrLn "DECRYPTED CONTENTS:"
            print $ unvigenere key input

parse :: [String] -> IO Vigenere
parse ["-e", "-k", key] = pure Vigenere { key, actionType = Encrypt }
parse ["-ek", key]      = pure Vigenere { key, actionType = Encrypt }
parse ["-d", "-k", key] = pure Vigenere { key, actionType = Decrypt }
parse ["-dk", key]      = pure Vigenere { key, actionType = Decrypt }
parse _                 = showUsage >> failExit

showUsage :: IO ()
showUsage = putStrLn "Usage: foo [-e/d] [-k] \"stringkey\""

exit :: IO a
exit = exitSuccess

failExit :: IO a
failExit = exitWith $ ExitFailure 1