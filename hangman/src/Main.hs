module Main where

import           Control.Monad (forever)
import           Data.Char     (toLower)
import           Data.List     (intersperse)
import           Data.Maybe    (isJust)
import           Data.Monoid   ((<>))
import           System.Exit   (exitSuccess)
import           System.Random (randomRIO)

main :: IO ()
main = putStrLn "hello world"

type WordList = [String]

allWords :: IO WordList
allWords = do
    dict <- readFile "data/words.txt"
    return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
    all_words <- allWords
    return $ filter gamelength all_words
    where
        gamelength w = let
            l = length w
            in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
    r <- randomRIO (0, length wl)
    return $ wl !! r

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
        intersperse ' ' (renderPuzzleChar <$> discovered) <> " Guessed so far: " <> guessed

freshPuzzle :: String -> Puzzle
freshPuzzle wrd = Puzzle wrd (map (const Nothing) wrd) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle wrd _ _) c = c `elem` wrd

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = c `elem` guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle wrd filled s) c =
    Puzzle wrd newFilled (c : s)
    where
        zipper guessed wordChar guessChar =
            if wordChar == guessed
            then Just wordChar
            else guessChar
        newFilled = zipWith (zipper c) wrd filled

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " <> [guess]
    case (charInWord puzzle guess
        , alreadyGuessed puzzle guess) of
            (_, True) -> do
                putStrLn "You already guessed that!"
                return puzzle
            (True, _) -> do
                putStrLn "Guessed correctly, filling in"
                return (fillInCharacter puzzle guess)
            (False, _) -> do
                putStrLn "No luck, try again"
                return (fillInCharacter puzzle guess)
