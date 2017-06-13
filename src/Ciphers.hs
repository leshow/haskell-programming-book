module Ciphers where

import           Control.Monad  (forever)
import           Data.Char
import           System.Exit    (exitSuccess)

caesar :: Int -> String -> String
caesar shift = map (shiftChar (+) shift)

shiftChar :: (Int -> Int -> Int) -> Int -> Char -> Char
shiftChar f shift char = chr normalize
    where
        normalize = mod (f num shift - 97) 26 + 97
        num = ord char
        -- unicode 'a' stars at 97

uncaesar :: Int -> String -> String
uncaesar shift = map (shiftChar (-) shift)

-- vigenere
-- my soln is technically not exactly the same as the book as I just jump the space char

normalizePos :: Char -> Int
normalizePos c = mod (ord c - 97) 26

vigen :: (Int -> Int -> Int) -> String -> String -> String
vigen f keyword = zipWith (mapKey f) $ cycle keyword
    where
        mapKey f kChar sChar = case sChar of
            ' ' -> ' '
            _   -> shiftChar f (normalizePos kChar) sChar

vigenere :: String -> String -> String
vigenere = vigen (+)

unvigenere :: String -> String -> String
unvigenere = vigen (-)

takeInput :: IO ()
takeInput = forever $ do
    putStrLn "write 1 for caesar 2 for vigenere cypher"
    select <- getLine
    case read select of
        1 -> do
            putStrLn "amount to shift (Integer)"
            shift <- getLine
            putStrLn "Write your message to be encrypted"
            msg <- getLine
            print $ caesar (read shift) msg
            exitSuccess
        2 -> do
            putStrLn "keyword to shift (String)"
            key <- getLine
            putStrLn "Write your message to be encrypted"
            msg <- getLine
            print $ vigenere key msg
            exitSuccess
        _ -> return ()
