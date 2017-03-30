{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase   #-}

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


swapEither :: Either e a -> Either a e
swapEither (Left e)  = Right e
swapEither (Right a) = Left a

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ swapEither <$> ema

swapEitherT' :: Monad m => EitherT e m a -> EitherT a m e
swapEitherT' (EitherT ema) = EitherT $ do
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
