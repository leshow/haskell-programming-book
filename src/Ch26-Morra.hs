{-# LANGUAGE RecordWildCards #-}


module Morra where

import           Control.Monad              (forever, when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Monoid                ((<>))
import           System.Exit                (exitSuccess)
import           System.Random

data Type = Odds | Evens deriving (Eq, Show)

newRand :: IO Int
newRand = randomRIO (0,1)

-- partially applied ReaderT
type Morra = ReaderT Config (StateT Game IO)

data Game = Game {
    humanScore :: Int
    , pcScore  :: Int
    , rounds   :: Int
}

data Config = Config {
    human :: Type
    , pc  :: Type
}

oddOrEven :: Int -> Type
oddOrEven r = if even r then Evens else Odds

initialGame :: Game
initialGame = Game 0 0 0

initConfig :: Type -> Type -> Config
initConfig hum comp = Config hum comp

getConfig :: IO Config
getConfig = do
    computer <- oddOrEven <$> newRand
    person <- oddOrEven <$> newRand
    return $ initConfig person computer

humanInc :: Game -> Game
humanInc (Game hs ps r) = Game (hs+1) ps (r+1)

pcInc :: Game -> Game
pcInc (Game hs ps r) = Game hs (ps+1) (r+1)


rollDie :: Morra ()
rollDie = do
    game@Game{..} <- lift get
    config@Config{..} <- ask
    humanRoll <- liftIO getLine
    liftIO $ putStrLn "Guess accepted."
    pcRoll <- liftIO $ randomRIO (0,1)
    let final = (read humanRoll :: Int) + pcRoll
        typeOf = oddOrEven final
        winner = typeOf == human
        (newGame, title) = if winner then (humanInc game, "human") else (pcInc game, "pc")
    liftIO $ print winner
    liftIO $ putStrLn ("Winner of this round is " <> title <> "!!")
    lift $ put newGame
    return ()


main :: IO ()
main = do
    config <- getConfig
    let game = initialGame
    forever $ runStateT (runReaderT rollDie config) game
