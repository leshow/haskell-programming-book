module Ch13 where

{-
    Import questions:
    1. Control.Monad imports forever, when in the example
    2. Data.Bits/Database.Blacktip.Types is imported in entirety, unqualified/
    3. the definition of the libraries types
    4.
        a) Control.Concurrent.MVar, Filesystem.Path.CurrentOS, Control.Concurrent
        b) Filesystem
        c) Control.Monad
-}

import           Control.Monad
import           Data.Char     (isAlpha, isSpace, toLower)
import           Data.Monoid   ((<>))
import           System.Exit

palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    let comp = format line1
    case (comp == reverse comp) of
        True  -> putStrLn "palindrome!"
        False -> do
            putStrLn "Not palindrome!"
            exitSuccess

format :: String -> String
format = foldr (\a xs -> if (isSpace a || not (isAlpha a)) then xs else (toLower a) : xs) ""

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid
    = NameEmpty
    | AgeTooLow
    | PersonInvalidUnknown String
    deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise = Left . PersonInvalidUnknown $
                "Name was: " ++ show name ++
                "Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
    putStr "Name: "
    name <- getLine
    putStr "Age: "
    age <- getLine
    let person = mkPerson name (read age)
    case person of
        Left err     -> putStr "Error! " <> print err
        Right person -> print person
