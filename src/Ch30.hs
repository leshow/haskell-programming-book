{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}

module Ch30 where

import Control.Exception (ArithException(..), AsyncException(..))
import Data.Typeable

data MyException = forall e. (Show e, Typeable e) => MyException e

instance Show MyException where
    showsPrec p (MyException e) = showsPrec p e

multiError :: Int -> Either MyException Int
multiError n = if | n == 0 -> Left $ MyException DivideByZero
                  | n == 1 -> Left $ MyException StackOverflow
                  | otherwise -> Right n