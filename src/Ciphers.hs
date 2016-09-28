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
