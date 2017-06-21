{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiWayIf                #-}

module Main where

import           Control.Exception (ArithException (..), AsyncException (..),
                                    SomeException (..), catch)
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
