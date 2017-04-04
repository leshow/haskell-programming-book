{-# LANGUAGE RecordWildCards #-}


module Morra where

import           Control.Monad             (forever)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           System.Random

data Type = Odds | Evens deriving (Eq, Show)

newRand :: IO Int
newRand = randomRIO (0,1)

type Morra = StateT Game IO
data Game = Game {
    human        :: Type
    , humanScore :: Int
    , pc         :: Type
    , pcScore    :: Int
    , rounds     :: Int
}

oddOrEven :: Int -> Type
oddOrEven r = case r of
    0 -> Evens
    1 -> Odds

initial :: Type -> Type -> Game
initial hum comp = Game hum 0 comp 0 0

main = do
    computer <- oddOrEven <$> newRand
    person <- oddOrEven <$> newRand
    let game = initial person computer
    forever $ runStateT rollDie game

humanInc :: Game -> Game
humanInc (Game h hs p ps r) = Game h (hs+1) p ps (r+1)

pcInc :: Game -> Game
pcInc (Game h hs p ps r) = Game h hs p (ps+1) (r+1)


rollDie :: StateT Game IO ()
rollDie = do
    game@Game{..} <- get
    humanRoll <- liftIO $ randomRIO (1,2)
    pcRoll <- liftIO $ randomRIO (1,2)
    let final = humanRoll + pcRoll
        typeOf = oddOrEven final
        newGame = if typeOf == human then humanInc game else pcInc game
    put newGame
    return ()
