{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiWayIf                #-}

module Main where

import           Control.Exception
import           Data.Typeable

data MyException = forall e. (Show e, Typeable e) => MyException e

instance Show MyException where
    showsPrec p (MyException e) = showsPrec p e

multiError :: Int -> Either MyException Int
multiError n = if | n == 0 -> Left $ MyException DivideByZero
                  | n == 1 -> Left $ MyException StackOverflow
                  | otherwise -> Right n


handler :: SomeException -> IO ()
handler (SomeException e) = do
    print (typeOf e)
    putStrLn $ "We errored! It was: " ++ show e


main :: IO ()
main = writeFile "zzz" "hi" `catch` handler

willIFail :: Integer -> IO (Either ArithException ())
willIFail denom = try $ print $ div 5 denom

onlyReportError :: Show e => IO (Either e a) -> IO ()
onlyReportError action = do
    res <- action
    case res of
        Left e  -> print e
        Right _ -> return ()

willFail :: Integer -> IO ()
willFail denom = onlyReportError $ willIFail denom

willIFail' :: Integer -> IO ()
willIFail' denom = print (div 5 denom) `catch` handler'
    where handler' :: ArithException -> IO ()
          handler' = print


