{-# LANGUAGE BangPatterns #-}

module BinaryStrings (runBench, binaryStrings, binaryBS) where


import           Criterion.Main
import qualified Data.ByteString       as BS
import           Data.ByteString.Char8 (pack)
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

runBench :: IO ()
runBench =
    let -- we don't want to benchmark packing
        !_one = pack "1?101010?010"
        !_two = pack "1????01?1010"
        !_three = pack "1??????????0"
        !_four = pack "????????????"
    in
        defaultMain [
    bgroup "strings" [
            bench "1?101010?010"  $ nf binaryStrings "1?101010?010"
            , bench "1????01?1010"  $ nf binaryStrings "1????01?1010"
            , bench "1??????????0"  $ nf binaryStrings "1??????????0"
            , bench "????????????"  $ nf binaryStrings "????????????"
            ],
        bgroup "bytestrings" [
            bench "1?101010?010" $ nf binaryBS _one
            , bench "1????01?1010" $ nf binaryBS _two
            , bench "1??????????0"  $ nf binaryBS _three
            , bench "????????????"  $ nf binaryBS _four
            ]
    ]

-- It appears to be that for small lists they perform about evenly
-- However, the larger the list, the more the bytestring approach
-- appears to be useful, even in the face of forcing strict evaluation
-- on String.

-- My guess is that since ByteStrings are vectors of Word8 elements
-- their allocation is more expensive, and is paid upfront,
-- therefore, you must make the string sufficiently long in order
-- to offset the cost of allocation.
