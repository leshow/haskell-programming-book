module Main where

import           Control.Monad (forever, when)
import           Data.Char     (toLower)
import           Data.List     (intersperse)
import           Data.Maybe    (catMaybes, isJust, isNothing)
import           Data.Monoid   ((<>))
import           System.Exit   (exitSuccess)
import           System.Random (randomRIO)

main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle (toLower <$> word)
    runGame puzzle


-- WORDS
newtype Wordlist = Wordlist [String]
    deriving (Eq, Show)

totalGuesses :: Int
totalGuesses = 3

allWords :: IO Wordlist
allWords = do
    dict <- readFile "data/words.txt"
    return $ Wordlist (lines dict)

minWordLength :: Int
minWordLength = 4

maxWordLength :: Int
maxWordLength = 8

gameWords :: IO Wordlist
gameWords = do
    (Wordlist all_words) <- allWords
    return $ Wordlist (filter gamelength all_words)
    where
        gamelength w = let
            l = length w
            in l > minWordLength && l < maxWordLength

randomWord :: Wordlist -> IO String
randomWord (Wordlist wl) = do
    r <- randomRIO (0, length wl)
    return $ wl !! r

randomWord' :: IO String
randomWord' = gameWords >>= randomWord


-- PUZZLE
type GuessesLeft = Int
--                  toGuess representation guessed
data Puzzle = Puzzle String [Maybe Char] [Char] GuessesLeft

instance Show Puzzle where
    show (Puzzle _ discovered guessed _) =
        intersperse ' ' (renderPuzzleChar <$> discovered) <> " Guessed so far: " <> guessed

freshPuzzle :: String -> Puzzle
freshPuzzle wrd = Puzzle wrd (map (const Nothing) wrd) [] 0

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle wrd _ _ _) c = c `elem` wrd

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed _) c = c `elem` guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle wrd filled s g) c =
    Puzzle wrd newFilled (c : s) g
    where
        zipper guessed wordChar guessChar =
            if wordChar == guessed
            then Just wordChar
            else guessChar
        newFilled = zipWith (zipper c) wrd filled

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle@(Puzzle a b c g) guess = do
    putStrLn $ "Your guess was: " <> [guess]
    case (charInWord puzzle guess
        , alreadyGuessed puzzle guess) of
            (_, True) -> do
                putStrLn "You already guessed that!"
                return puzzle
            (True, _) -> do
                putStrLn "Guessed correctly!"
                return (fillInCharacter (Puzzle a b c g) guess)
            (False, _) -> do
                putStrLn "No luck, try again"
                return (fillInCharacter (Puzzle a b c (g+1)) guess)

remainingGuesses :: Puzzle -> Int
remainingGuesses (Puzzle _ filled guessed _) =
    length (cancelLists guessed successfulChars)
    where
        successfulChars = catMaybes filled

cancelLists ys = foldr (\a b -> if a `elem` ys then b else a : b) []

gameOver :: Puzzle -> IO ()
gameOver p@(Puzzle wordToGuess filled guessed g) =
        when (length guessed > g) $
            do
                putStrLn "You lose"
                putStrLn $ "The word was " <> wordToGuess
                exitSuccess

gameWin :: Puzzle -> IO ()
gameWin puzzle@(Puzzle _ filledInSoFar _ _) =
    Control.Monad.when (all isJust filledInSoFar) $
        do  putStrLn "You win!"
            print puzzle
            exitSuccess

-- forever used to run game loop
runGame :: Puzzle -> IO ()
runGame puzzle = Control.Monad.forever $ do
    gameWin puzzle
    gameOver puzzle
    putStrLn $ "Current puzzle is: " <> show puzzle
    --print (remainingGuesses puzzle)
    putStr "Guess a letter: "
    guess <- getLine
    handleInput puzzle guess

-- respond to different inputs
handleInput :: Puzzle -> String -> IO ()
handleInput puzzle s = case s of
    [c] -> do                   -- handleGuess puzzle c >>= runGame
        pz <- handleGuess puzzle c
        runGame pz
    _   -> putStrLn "Your guess must be a single char"

-- for fun, try to construct a hangman DSL using Free
