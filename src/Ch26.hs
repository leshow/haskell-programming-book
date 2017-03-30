{-# LANGUAGE InstanceSigs #-}

module Ch26 where

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m  => Functor (MaybeT m) where
    fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
    pure x = MaybeT (pure (pure x))
    (MaybeT fab) <*> (MaybeT mma) = MaybeT $ (<*>) <$> fab <*> mma

instance (Monad m) => Monad (MaybeT m) where
    return = pure
    (>>=):: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    (MaybeT ma) >>= f = MaybeT $ do
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
    (EitherT ema) >>= f = EitherT $ do
        u <- ema -- u :: Either e a
        case u of
            Left e  -> return $ Left e
            Right a -> runEitherT $ f a

