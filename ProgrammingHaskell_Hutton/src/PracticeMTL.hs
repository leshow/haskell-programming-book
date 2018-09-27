module PracticeMTL where

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance ∀ m s. Functor m => Functor (StateT s m) where
    fmap :: ∀ a b. (a -> b) -> StateT s m a -> StateT s m b
    fmap f (StateT tupleS) = StateT $ \s ->
        let
            g (a', s') = (f a', s')
        in
            fmap g (tupleS s)

data Opt a = Some a | None

instance Functor Opt where
    fmap :: (a -> b) -> Opt a -> Opt b
    fmap _ None     = None
    fmap f (Some a) = Some $ f a

instance Applicative Opt where
    (<*>) :: Opt (a -> b) -> Opt a -> Opt b
    (<*>) None _       = None
    (<*>) (Some ab) fa = ab <$> fa
    pure :: a -> Opt a
    pure = Some

instance Monad Opt where
    return = pure
    (>>=) :: Opt a -> (a -> Opt b) -> Opt b
    Some a >>= f = f a
    None >>= _ = None

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance ∀ m. Functor m => Functor (MaybeT m) where
    fmap :: ∀ a b. (a -> b) -> MaybeT m a -> MaybeT m b
    fmap f (MaybeT mMaybea) = MaybeT $ (fmap . fmap) f mMaybea

instance (Functor m, Monad m) => Applicative (MaybeT m) where
    pure :: a -> MaybeT m a
    pure a = MaybeT $ pure (Just a)
    (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
    maybeMF <*> maybema = MaybeT $ do
        maybeF <- runMaybeT maybeMF
        case maybeF of
            Nothing -> pure Nothing
            Just f -> do
                maybea <- runMaybeT maybema
                case maybea of
                    Nothing -> pure Nothing
                    Just a  -> pure (Just (f a))

instance (Monad m) => Monad (MaybeT m) where
    return = pure
    (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    maybeMA >>= aMaybeMB = MaybeT $ do
        ma <- runMaybeT maybeMA
        case ma of
            Nothing -> pure Nothing
            Just a  -> runMaybeT (aMaybeMB a)
