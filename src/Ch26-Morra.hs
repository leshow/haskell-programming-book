{-# LANGUAGE RecordWildCards #-}


module Morra where

import           Control.Monad              (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Char                  (digitToInt, isDigit, isLetter)
import           Data.Monoid                ((<>))
import           System.Exit                (exitSuccess)
import           System.IO
import           System.Random


data Type = Odds | Evens deriving (Eq, Show)

data UserAction =
    Roll Int
    | Cmd Char
    deriving (Eq, Show)

type Morra a = ReaderT Config (StateT Game IO) a

data Game = Game {
    humanScore :: Int
    , pcScore  :: Int
    , rounds   :: Int
} deriving (Eq, Show)

data Config = Config {
    human :: Type
    , pc  :: Type
} deriving (Eq, Show)



newRand :: IO Int
newRand = randomRIO (0,1)

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

runGame :: Morra ()
runGame = do
    game@Game{..} <- lift get
    Config{..} <- ask
    input <- liftIO getInput
    case input of
        Roll num -> do
            liftIO $ putStrLn "Guess accepted."
            pcRoll <- liftIO $ randomRIO (0,1)
            let total = num + pcRoll
                typeOf = oddOrEven total
                winner = typeOf == human
                (newGame, title) = if winner then (humanInc game, "human") else (pcInc game, "pc")
            liftIO $ do
                putStrLn $ if winner then "Winner" else "Loser"
                putStrLn ("Winner of this round is " <> title <> "!!")
                putStrLn (show newGame)
            lift $ put newGame
            runGame
        Cmd c -> do
            liftIO $ when (c `elem` "Qq") $ do
                print game
                exitSuccess
            runGame

getInput :: IO UserAction
getInput = do
    helpText
    c <- getChar
    putStrLn " "
    return $ getDig c

getDig :: Char -> UserAction
getDig c
    | isDigit c = Roll (digitToInt c)
    | isLetter c = Cmd c

helpText :: IO ()
helpText = do
    putStrLn "Press '1' for Odd '2' for Even, or q for quit"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    config <- getConfig
    let game = initialGame
    runStateT (runReaderT runGame config) game
    return ()
