module Ciphers where

import           Data.Char


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
