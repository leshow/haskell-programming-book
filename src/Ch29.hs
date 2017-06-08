{-# LANGUAGE LambdaCase                 #-}
 -- \case of { ... }
{-# LANGUAGE MultiWayIf                 #-}
 -- if | guard -> expr; | guard2 -> expr2
{-# LANGUAGE ParallelListComp           #-}
 -- adds zipWith to list compr. [(x+y) | x <- [1..10] | y <- [11..20]] -> zipWith (+) [1..10] [11..20]
{-# LANGUAGE NamedFieldPuns             #-}
 -- data C = C { a :: Int } when updated becomes C {a} instead of C {a=a}
{-# LANGUAGE RecordWildCards            #-}
 -- update only necessary field,
-- let {a = 1; b = 2; c = 3; d = 4} in C {..}  -- or on the other side -- f (C {a = 1, ..}) = b + c + d
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
 -- allow newtype's to derive more than just Eq/Ord/Enum/Bounded
{-# LANGUAGE PatternSynonyms            #-}
-- allows you to construct arbitratry patterns,
{-# LANGUAGE FunctionalDependencies     #-}
-- class Foo a b c | a b -> c where ...
-- {-# LANGUAGE ExplicitForAll             #-}
-- self explanatory
-- {-# LANGUAGE OverloadedLists            #-}
-- have a card for this
-- {-# LANGUAGE OverloadedStrings          #-}
-- have a card for this also
-- {-# LANGUAGE RankNTypes                 #-}
-- f2 :: (forall a. a -> a) -> Int -> Int
-- f3 :: Int -> (forall a. a -> a)
-- allows polymorphism not constrained to callers types
-- {-# LANGUAGE Strict #-} -- makes everything strict except ~
-- {-# LANGUAGE BangPatterns #-} -- allows ! to force strictness

module Main where

import           Control.Applicative (liftA2)
import           Control.Concurrent  (MVar, forkIO, newEmptyMVar, putMVar,
                                      takeMVar, threadDelay)
import           Control.Monad       (replicateM)
import           System.Random       (randomIO, randomRIO)

main :: IO ()
main = do
    mv <- newEmptyMVar
    putMVar mv 0
    forkIO (forked mv)
    threadDelay 1000
    a <- takeMVar mv
    print a
    a' <- takeMVar mv
    print a'
    liftA2 (++) getLine getLine -- use applicative IO instance
    pure ()

forked :: (Show a, Num a) => MVar a -> IO ()
forked mv = do
    a <- takeMVar mv
    putMVar mv (a+1)
    threadDelay 1000000
    putMVar mv (a+1)
    pure ()

gimmeShelter :: Bool -> IO [Int]
gimmeShelter True  = replicateM 10 (randomRIO (0, 10))
gimmeShelter False = pure [0]


-- behaves like data Point = Point { x :: Int, y :: Int }
pattern Point :: Int -> Int -> (Int, Int)
pattern Point {x,y} = (x,y)

data Type = App String [Type]


pattern Arrow t1 t2 = App "->" [t1,t2]
pattern TInt = App "Int" []

recursive = Arrow TInt TInt --eq.  App "->" [App "Int" [], App "Int" []]

class Coll s a | s -> a where
  empty  :: s
  insert :: s -> a -> s
