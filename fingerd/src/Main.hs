{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Exception
import           Control.Monad                (forever)
import           Control.Monad.Reader
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as C
import           Data.Foldable
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Encoding           (decodeUtf8', encodeUtf8)
import           Data.Typeable
import           Database.SQLite.Simple       hiding (close)
import qualified Database.SQLite.Simple       as SQL
import           Database.SQLite.Simple.Types
import qualified Network.Socket               as NS
import           Network.Socket.ByteString    (recv, sendAll)
import           System.IO                    (stderr)
import           Text.RawString.QQ

data User = User
    { userId        :: Integer
    , username      :: Text
    , shell         :: Text
    , homeDirectory :: Text
    , realName      :: Text
    , phone         :: Text
    } deriving (Eq, Show)

data Env = Env
    { fconn :: Connection
    , fsock :: NS.Socket
    , port  :: BS.ByteString
    }

class HasConn env where
    getConn :: env -> Connection

instance HasConn Env where
    getConn = fconn

class HasFSock env where
    getSock :: env -> NS.Socket

instance HasFSock Env where
    getSock = fsock

class HasPort env where
    getPort :: env -> BS.ByteString

instance HasPort Env where
    getPort = port

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
    toRow (User id_ username shell homeDirectory realName phone) =
        toRow (id_, username, shell, homeDirectory, realName, phone)

createUsers :: Query
createUsers = [r|
    CREATE TABLE IF NOT EXISTS users
        (id INTEGER PRIMARY KEY AUTOINCREMENT,
        username TEXT UNIQUE,
        shell TEXT, homeDirectory TEXT,
        realName TEXT, phone TEXT)
|]

allUsers :: Query
allUsers = "SELECT * FROM users"

byUsername :: Query
byUsername = "SELECT * FROM users WHERE username = ?"

insertUser :: Query
insertUser = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn name = do
    res <- query conn byUsername (Only name)
    case res of
        []     -> pure Nothing
        [user] -> pure $ Just user
        _      -> throwIO DuplicateData

createDatabase :: IO ()
createDatabase = do
    conn <- open "finger.db"
    SQL.execute_ conn createUsers
    SQL.execute conn insertUser me
    rows <- SQL.query_ conn allUsers
    traverse_ print (rows :: [User])
    SQL.close conn
    where
        me :: UserRow
        me = (Null, "Evan", "/bin/zsh", "/home/leshow", "Evan Cameron", "555-5555")

returnUsers :: Connection -> NS.Socket -> IO ()
returnUsers conn soc = do
    userRows <- SQL.query_ conn allUsers
    let usernames = fmap username userRows
        bsUsers = foldr (\a acc -> T.concat [acc, a, "\n"]) "" usernames
    sendAll soc (encodeUtf8 bsUsers)

formatUser :: User -> BS.ByteString
formatUser (User _ username shell homeDir realName _) = BS.concat [
    "Login: ", enc username, "\t\t\t\t",
    "Name: ", enc realName, "\n",
    "Directory: ", enc homeDir, "\t\t\t\t",
    "Shell: ", enc shell, "\n"]
    where
        enc = encodeUtf8

returnUser :: Connection -> NS.Socket -> Text -> IO ()
returnUser conn soc username = do
    maybeUser <- getUser conn (T.strip username)
    case maybeUser of
        Nothing -> do
            putStrLn $ "Couldn't find matching user for username: " <> show username
            pure ()
        Just user -> sendAll soc (formatUser user)



handleQuery :: Connection -> NS.Socket -> IO ()
handleQuery conn soc = do
    msg <- recv soc 2014
    case msg of
        "\r\n" -> returnUsers conn soc
        name   -> do
            let res = decodeUtf8' name
            case res of
                Left err -> BS.hPutStr stderr $ C.pack ("Utf8 error: " ++ show err)
                Right n  -> returnUser conn soc n

handleQueries :: Connection -> NS.Socket -> IO ()
handleQueries conn sock = forever $ do
    (soc, _) <- NS.accept sock
    putStrLn "Got connection, handling query"
    handleQuery conn soc
    NS.close soc

main :: IO ()
main = NS.withSocketsDo $ do
    addrinfo <- NS.getAddrInfo (Just (NS.defaultHints {NS.addrFlags = [NS.AI_PASSIVE]})) Nothing (Just "7779")
    let serveraddr = head addrinfo
    sock <- NS.socket (NS.addrFamily serveraddr) NS.Stream NS.defaultProtocol
    NS.bind sock (NS.addrAddress serveraddr)
    NS.listen sock 1
    conn <- open "finger.db"
    handleQueries conn sock
    SQL.close conn
    NS.close sock
