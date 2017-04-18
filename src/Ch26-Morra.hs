{-# LANGUAGE RecordWildCards #-}


module Morra where

import           Control.Monad              (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Char                  (digitToInt, isDigit, isLetter)
import qualified Data.IntMap.Strict         as M
import           Data.Monoid                ((<>))
import           System.Exit                (exitSuccess)
import           System.IO
import           System.Random


data Type = Odds | Evens deriving (Eq, Show, Enum)

data UserAction =
    Roll Int
    | Cmd Char
    deriving (Eq, Show)

type Morra a = ReaderT Config (StateT Game IO) a

data Game = Game {
    humanScore :: Int
    , pcScore  :: Int
    , rounds   :: Int
    , turns    :: [Round]
} deriving Show

data Config = Config {
    human :: Type
    , pc  :: Type
} deriving Show

data Round = Round {
    p1   :: Type
    , p2 :: Type
} deriving (Show, Eq)

newRand :: IO Int
newRand = randomRIO (0,1)

oddOrEven :: Int -> Type
oddOrEven r = if even r then Evens else Odds

initialGame :: Game
initialGame = Game 0 0 0 []

getConfig :: IO Config
getConfig = do
    computer <- oddOrEven <$> newRand
    person <- oddOrEven <$> newRand
    return $ Config person computer

humanInc :: Game -> Round -> Game
humanInc g r = g { humanScore = humanScore g + 1, rounds = rounds g + 1, turns = r : turns g }

pcInc :: Game -> Round -> Game
pcInc g r = g { pcScore = pcScore g + 1, rounds = rounds g + 1, turns = r : turns g }

runGame :: Morra ()
runGame = do
    game@Game{..} <- lift get
    Config{..} <- ask
    input <- liftIO getInput
    case input of
        Roll num -> do
            liftIO $ putStrLn "Guess accepted."
            pcRoll <- liftIO (aiRoll turns)
            let total = num + pcRoll
                typeOf = oddOrEven total
                winner = typeOf == human
                turn = Round { p1 = oddOrEven num, p2 = oddOrEven pcRoll }
                (newGame, title) = if winner then (humanInc game turn, "human") else (pcInc game turn, "pc")
            liftIO $ do
                putStrLn $ if winner then "Winner" else "Loser"
                putStrLn ("Winner of this round is " <> title <> "!!")
                print newGame
            lift $ put newGame
            runGame
        Cmd c -> do
            liftIO $ when (c `elem` "Qq") $ do
                printWinner game
                exitSuccess
            runGame

aiRoll :: [Round] -> IO Int
aiRoll rounds =
    if length rounds < 3
    then newRand
    else
        let rs = p1 <$> take 3 rounds
            roundMap = foldr (\a acc -> M.insertWith (+) (fromEnum a) 1 acc) M.empty rs
        in
            do
                print (M.toList roundMap)
                newRand


printWinner :: Game -> IO ()
printWinner g@Game{ humanScore = h, pcScore = p } = do
    if h > p then putStrLn "Humanity wins! We hold off the machines a while longer."
    else putStrLn "Computer wins!"
    print g

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
helpText = putStrLn "Press '1' for Odd '2' for Even, or q for quit"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    config <- getConfig
    let game = initialGame
    runStateT (runReaderT runGame config) game
    return ()
