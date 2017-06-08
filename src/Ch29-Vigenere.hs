module Main where

--import           Ciphers            (unvigenere, vigenere)
import           System.Environment
import           System.Exit

main :: IO ()
main = do
    args <- getArgs
    file <- parse args
    print file
    pure ()

parse :: [String] -> IO String
parse ["-e", file] = pure file
parse ["-d", file] = pure file
parse _            = showUsage >> failExit

showUsage :: IO ()
showUsage = putStrLn "Usage: foo [-ed] file"

exit :: IO a
exit = exitWith ExitSuccess

failExit :: IO a
failExit = exitWith (ExitFailure 1)
