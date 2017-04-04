{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Ch26ScottyEx where

import           Control.Concurrent.MVar    (MVar, newMVar, putMVar, takeMVar)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as T
import           System.Environment         (getArgs)
import           Web.Scotty.Trans


data AppConfig = AppConfig {
    counts   :: MVar (M.Map Text Integer)
    , prefix :: Text
}

type Scotty = ScottyT Text (ReaderT AppConfig IO)
type Handler = ActionT Text (ReaderT AppConfig IO)

bumpBoop :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoop k m =
    let
        n = M.insertWith (+) k 1 m
    in
        (n, fromMaybe 1 (M.lookup k m))
    -- let
    --     v = fromMaybe 0 (M.lookup k m) + 1
    -- in
    --     (M.insert k v m, v)



app :: Scotty ()
app = get "/:key" $ do
    unprefixed <- param "key"
    AppConfig{..} <- lift ask
    m <- liftIO $ takeMVar counts -- get map
    let key' = prefix `mappend` unprefixed
        (m', v) = bumpBoop key' m -- update map with key
    liftIO $ putMVar counts m' -- put map back in mvar
    html $ mconcat ["<h1> Success! Count was: "
                    , T.pack $ show v
                    , "</h1>"]


main :: IO ()
main = do
    [prefixArg] <- getArgs
    counter <- newMVar M.empty
    let
        config = AppConfig counter (T.pack prefixArg)
        runR r = runReaderT r config
    scottyT 4000 runR app
