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
import           Database.SQLite.Simple
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

data DuplicateData = DuplicateData deriving (Eq, Show, Typeable)

instance Exception DuplicateData

type UserRow = (Null, Text, Text, Text, Text, Text)

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

createUsers :: Query
createUsers = [r|
    CREATE TABLE IF NOT EXISTS users
        (id INTEGER PRIMARY KEY AUTOINCREMENT,
        username TEXT UNIQUE,
        shell TEXT, homeDirectory TEXT,
        realName TEXT, phone TEXT)
|]

allUsers :: Query
allUsers = "SELECT * FROM USERS"

byUsername :: Query
byUsername = "SELECT * FROM USERS WHERE username = ?"

insertUser :: Query
insertUser = "INSERT INTO users WHERE VALUES (?, ?, ?, ?, ?, ?)"

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn name = do
    res <- query conn byUsername (Only name)
    case res of
        []     -> pure Nothing
        [user] -> pure $ Just user
        _      -> throwIO DuplicateData

createDatabase :: IO ()
createDatabase = do
    conn <- open "test.db"
    execute_ conn createUsers
    execute conn insertUser me
    rows <- query_ conn allUsers
    mapM_ print (rows :: [User])
    Database.SQLite.Simple.close conn
    where
        me :: UserRow
        me = (Null, "Evan", "/bin/zsh", "/home/leshow", "Evan Cameron", "555-5555")

main :: IO ()
main = do
  putStrLn "hello world"
