{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Single where

import           Data.Singletons
import           Data.Singletons.TH
import           Data.Kind
import           Prelude hiding (replicate)

$(singletons [d|
  data Nat = Z | S Nat
    deriving (Show, Eq)
  
  (+) :: Nat -> Nat -> Nat
  (+) Z m = m
  (+) (S n) m = S (n + m)

  (*) :: Nat -> Nat -> Nat
  (*) Z _ = Z
  (*) (S m) n = n * m + m
 
  |])

data List a n where
    Nil :: List a 'Z
    (:-) :: a -> List a n -> List a ('S n)

infixr 5 :-

deriving instance Eq a => Eq (List a n)
deriving instance Show a => Show (List a n)

toList :: List a n -> [a]
toList Nil = []
toList (x :- xs) = x : toList xs

replicate :: SNat n -> a -> List a n
replicate SZ _ = Nil
replicate (SS n) x = x :- replicate n x

-- four :: Sing ('S ('S ('S ('S 'Z))))
four = SS (SS (SS (SS SZ)))

tryReplicate = do
    putStr "replicate (SS (SS (SS (SS SZ)))) 1 == "
    print $ replicate four 1 