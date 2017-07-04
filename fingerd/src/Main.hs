{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Exception
import           Control.Monad                (forever)
import qualified Data.ByteString              as BS
import           Data.List                    (intersperse)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Text.Encoding           (decodeUtf8, encodeUtf8)
import           Data.Typeable
import           Database.SQLite.Simple       hiding (close)
import           Database.SQLite.Simple.Types
import           Network.Socket               hiding (close, recv)
import           Network.Socket.ByteString    (recv, sendAll)
import           Text.RawString.QQ


data User = User
    { userId        :: Integer
    , username      :: Text
    , shell         :: Text
    , homeDirectory :: Text
    , realName      :: Text
    , phone         :: Text
    } deriving (Eq, Show)


instance FromRow User where
    fromRow = User <$> field
                <*> field
                <*> field
                <*> field
                <*> field
                <*> field

instance ToRow User where
    toRow (User id_ username shell homeDir realName phone) =
        toRow (id_, username, shell, homeDir, realName, phone)

main :: IO ()
main = do
  putStrLn "hello world"
