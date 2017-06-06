{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Text.Encoding     as TE
import qualified Data.Text.IO           as TIO

input :: BL.ByteString
input = "123"

compressed :: BL.ByteString
compressed = GZip.compress input

dictBS :: IO BL.ByteString
dictBS = BL.readFile "/usr/share/dict/words"

compressDict :: IO BL.ByteString
compressDict = do
    dict <- dictBS
    return $ GZip.compress dict

main :: IO ()
main = do
    TIO.putStrLn $ TE.decodeUtf8 (s input)
    where s = BL.toStrict
