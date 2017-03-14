{-# LANGUAGE InstanceSigs #-}

module Ch23Ex where

newtype State s a = State { runState :: s -> (a, s) }
-- newtype State s a = State { execState :: s -> (a, s) }

get :: State s s
get = State $ \x -> (x, x) -- equivalent to State $ \x -> (x, x)

put :: s -> State s ()
put s = State $ \_ -> ((), s) -- const ((), s)

-- type sig below is supposed ot just be State
exec :: State s a -> s -> s
exec (State sa) s = let (_, s') = sa s
                    in  s'

eval :: State s a -> s -> a
eval (State sa) s = let (a, _) = sa s
                    in a

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap f (State g) = State $ \x -> (f (fst (g x)), snd $ g x)
    --  let (a, s) = g x
    -- in (f a, s)

instance Applicative (State s) where
    pure :: a -> State s a
    pure a = State $ \s -> (a, s)

    (State f) <*> (State g) = State $ \x -> let (a, s) = f x
                                                (a', s') = g s
                                            in (a a', s')

instance Monad (State s) where
    return = pure
    (>>=) :: State s a -> (a -> State s b) -> State s b
    (State f) >>= g = State $ \x -> let (a, s) = f x
                                        (State a') = g a
                                    in a' s
