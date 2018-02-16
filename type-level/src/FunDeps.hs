{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module FunDeps where

data Nil
data Cons x xs

class Head list xs | list -> xs
instance Head Nil Nil
instance Head (Cons x more) x

class Append a b c | a b -> c
instance Append Nil x x
instance (Append as bs cs) => Append (Cons a as) bs (Cons a cs)

data False
data True

class AnyTrue list t | list -> t
instance AnyTrue Nil False
instance AnyTrue (Cons True xs) True
instance (AnyTrue list t) => AnyTrue (Cons False list) t

-- type family Append a b where
--     Append Nil xs = xs
--     Append (Cons x xs) ys = Append xs (Cons x ys)

class Or a b c | a b -> c
instance Or True False True
instance Or False True True
instance Or False False False
instance Or True True True

class Not a b | a -> b
instance Not False True
instance Not True False

data Z
data S a

type N0 = Z
type N1 = S N0
type N2 = S N1
type N3 = S N2
type N4 = S N3
type N5 = S N4
type N6 = S N5
type N7 = S N6
type N8 = S N7

class PEqual a b t | a b -> t
instance PEqual Z Z True
instance PEqual Z (S b) False
instance PEqual (S a) Z False
instance (PEqual a b t) => PEqual (S a) (S b) t

class LT a b t | a b -> t
instance LT Z Z False
instance LT Z (S b) True
instance LT (S a) Z False
instance (LT a b t) => LT (S a) (S b) t

class Range n xs | n -> xs
instance Range Z Nil
instance (Range n xs) => Range (S n) (Cons n xs)

class AddC m n r | m n -> r
instance AddC Z n n
instance AddC m n r => AddC (S m) n (S r)


class LegalCompare t | -> t
    where legalCompare :: t

instance (PEqual (S Z) (S Z) t) => LegalCompare t
-- compare that 1 == 1
-- stack ghci
-- :l src/FunDeps.hs
-- :type legalCompare
--  legalCompare :: True

-- type level $
class Apply f a r | f a -> r

