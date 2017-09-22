{-# LANGUAGE BangPatterns #-}

module BinaryStrings where


import           Criterion.Main
import qualified Data.ByteString       as BS
import           Data.ByteString.Char8 (pack, singleton)
import           Data.Word

-- generate all binary strings from a given pattern
-- so 1?0 becomes: [110, 100]
binaryStrings :: String -> [String]
binaryStrings = go ""
        where
          go :: String -> String -> [String]
          go cur "" = [reverse cur]
          go cur (x:xs)
            | x == '?'  = go ('1':cur) xs ++ go ('0': cur) xs
            | otherwise = go (x:cur) xs

-- should be faster underlying representation than String (a linked list of Chars)
binaryBS :: BS.ByteString -> [BS.ByteString]
binaryBS = go BS.empty
        where
            go :: BS.ByteString -> BS.ByteString -> [BS.ByteString]
            go !cur !xs = case BS.length xs of
                0 -> [BS.reverse cur]
                _ -> let
                        !h = BS.head xs
                        !t = BS.tail xs
                     in
                        if h == bsq
                            then go (BS.cons one cur) t ++ go (BS.cons zero cur) t
                            else go (BS.cons h cur) t

bsq :: Word8
!bsq = 0x3f

one :: Word8
!one = 0x31

zero :: Word8
!zero = 0x30

bench = defaultMain [
    bgroup "strings" [
            bench "1??10"  $ whnf binaryStrings "1??10"
            , bench "1????01?0"  $ whnf binaryStrings "1????01?0"
            ],
        bgroup "bytestrings" [
            bench "1??10" $ whnf binaryBS (pack "1??10")
            , bench "1????01?0" $ whnf binaryBS (pack "1????01?0")
            ]
    ]
