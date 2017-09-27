{-# LANGUAGE OverloadedStrings #-}

module Concurrently where

import           Control.Concurrent.Async
import           Data.Aeson               (toJSON)
import           Network.Wreq             (get)

import           BinaryStrings            (binaryBS, binaryStrings)
import           Data.ByteString.Char8    (pack)

doBoth :: IO ()
doBoth = do
    r' <- async (get "http://httpbin.org/get")
    r'' <- async (get "http://httpbin.org/get")
    p' <- wait r'
    p'' <- wait r''
    print p'
    print p''

-- this way if one throws an exception, the second will be killed
dowithfailure :: IO ()
dowithfailure =
    withAsync (get "http://httpbin.org/get") $ \a1 -> do
        withAsync (get "http://httpbin.org/get") $ \a2 -> do
            page1 <- wait a1
            page2 <- wait a2
            print page1 >> print page2

-- async has the combinator 'concurrently' for this scenario
