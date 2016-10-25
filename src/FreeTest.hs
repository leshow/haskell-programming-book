-- {-# LANGUAGE DeriveFunctor #-}
module FreeTest where

import           Control.Applicative
import           Control.Monad.Free
-- this is defined in Control.Monad.Free
-- data Free f a
--     = Free (f (Free f a))
--     | Pure a
--
-- instance Functor f => Functor (Free f) where
--     fmap f (Pure a) = Pure $ f a
--     fmap f (Free a) = Free $ fmap (fmap f) a
--
-- instance Functor f => Applicative (Free f) where
--     pure = Pure
--     (Free f) <*> a = Free $ fmap (<*> a) f
--
-- instance Functor f => Monad (Free f) where
--     return = Pure
--     (>>=) (Free a) f = Free (fmap (>>= f) a)
--     Pure a >>= f = f a
-- end def


-- define an algebra for programs that read and write to the terminal
data TeletypeF a
    = PrintLine String a
    | GetLine (String -> a)
-- deriving (Functor)

instance Functor TeletypeF where
    fmap f (PrintLine s a) = PrintLine s (f a)
    fmap f (GetLine g)     = GetLine (f . g)

getL :: Free TeletypeF String
getL = liftF (GetLine id)

printL :: String -> Free TeletypeF ()
printL s = liftF (PrintLine s ())

runTeletype :: Free TeletypeF a -> IO a
runTeletype =
    iterM $ \op ->
        case op of
            GetLine f     -> getLine >>= f
            PrintLine s f -> putStrLn s >> f
-- these 2 run functions are equivalent
-- iterM gives some convenience for writing
run :: Free TeletypeF a -> IO a
run (Pure r)               = return r
run (Free (PrintLine s a)) = putStrLn s >> run a
run (Free (GetLine f))     = getLine >>= run . f

-- now we can use our algebra
echo :: Free TeletypeF ()
echo = do
    s <- getL
    printL s

doThis = runTeletype echo
