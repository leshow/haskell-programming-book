{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Exception
import           Control.Monad      (forever)
import           Data.List          (intersperse)
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)


main :: IO ()
main = do
  putStrLn "hello world"
