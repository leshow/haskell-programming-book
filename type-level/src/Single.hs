{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Single where

import           Data.Singletons
import           Data.Singletons.TH
-- import GHC.Types
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

tail :: List a ('S n) -> List a n
tail (_ :- xs) = xs

head :: List a ('S n) -> a
head (x :- _) = x

lmap :: (a -> b) -> List a n -> List b n
lmap _ Nil = Nil
lmap f (x :- xs) = f x :- lmap f xs

type family Map (f :: Type -> Type) (xs :: List Type n) :: List Type n where
    Map f 'Nil = 'Nil
    Map f (x ':- xs) = f x ':- (Map f xs)


type family Range (n :: Nat) :: [Nat] where
    Range 'Z = '[] 
    Range ('S n) = n ': Range n 