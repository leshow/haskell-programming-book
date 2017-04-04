{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase   #-}

module Ch26 where

import           Control.Monad             (liftM)
import           Control.Monad.Trans.Class
import           Data.Bifunctor            (bimap)

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m  => Functor (MaybeT m) where
    fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
    pure x = MaybeT (pure (pure x))
    (MaybeT fab) <*> (MaybeT mma) = MaybeT $ (<*>) <$> fab <*> mma

instance (Monad m) => Monad (MaybeT m) where
    return = pure
    (>>=):: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    (MaybeT ma) >>= f = MaybeT $
        do
            v <- ma -- ma :: m (Maybe a),  v :: Maybe a
            case v of
                Nothing -> return Nothing
                Just y  -> runMaybeT $ f y


newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema

instance Applicative m => Applicative (EitherT e m) where
    pure x = EitherT (pure (pure x))
    (EitherT f) <*> (EitherT a) = EitherT $ (<*>) <$> f <*> a

instance Monad m => Monad (EitherT e m) where
    return = pure
    (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
    (EitherT ema) >>= f = EitherT $
        do
            u <- ema -- u :: Either e a
            case u of
                Left e  -> return $ Left e
                Right a -> runEitherT $ f a


swapEither :: Either e a -> Either a e
swapEither (Left e)  = Right e
swapEither (Right a) = Left a

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ swapEither <$> ema

swapEitherT' :: Monad m => EitherT e m a -> EitherT a m e
swapEitherT' (EitherT ema) = EitherT $
    do
        u <- ema
        return $ swapEither u


either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a)  = f a
either' _ g (Right b) = g b

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT amb) = amb >>= \case
    Left e -> f e
    Right a -> g a
    -- do
    --     u <- amb
    --     case u of
    --         Left e  -> f e
    --         Right a -> g a


newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
    fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
    pure a = ReaderT (pure (pure a))
    ReaderT fmab <*> (ReaderT rma) = ReaderT $ (<*>) <$> fmab <*> rma

instance Monad m => Monad (ReaderT r m) where
    return = pure
    (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
    ReaderT rma >>= f = ReaderT $
        \r -> do
            a <- rma r  -- rma :: r -> m a      rma r :: m a, then we unwrap m with (<-)
            runReaderT (f a) r


newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
    fmap :: (a -> b) -> StateT s m a -> StateT s m b
    fmap f (StateT sma) = StateT $
        \s ->
            let mas = sma s  -- sma :: s -> m (a,s)   sma s :: m (a, s)
            in bimap f id <$> mas    -- lift f into (a, _) as (f a, _)

instance Monad m => Applicative (StateT s m) where
    pure a = StateT $ \s -> pure (a, s)

    (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
    StateT fma <*> StateT ma = StateT $
        \s -> do
            (f, s') <- fma s  --
            (f', s'') <- ma s'
            return (f f', s'')


instance Monad m => Monad (StateT s m) where
    return = pure

    (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
    StateT sma >>= f = StateT $
        \s -> do
            (a, s') <- sma s
            (runStateT $ f a) s'

-- when we have something like type Type a = One -> Maybe (a, Two)
-- we can wewrite it like type Type = StateT One Two


instance MonadTrans (EitherT e) where
    lift m = EitherT (liftM Right m)


instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> do
        x <- m
        return (x,s)
