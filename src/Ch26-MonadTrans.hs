{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE UndecidableInstances #-}

module MonadTrans where


import           Control.Monad             (liftM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

newtype IdentityT m a = IdentityT { runIdentityT :: m a }
    deriving (Eq, Show)


-- the purpose of MonadTrans is to lift an action into our transformed monad
-- i.e. into IdentityT in this case
instance MonadTrans IdentityT where
    lift = IdentityT

instance MonadTrans MaybeT where
    lift = MaybeT . liftM Just

instance MonadTrans (ReaderT r) where
    lift = ReaderT . const

instance MonadTrans (EitherT e) where
    lift = EitherT . liftM Right -- liftM :: Monad m => (a -> r) -> m a -> m r -- is the same as fmap, but in Monad

instance MonadTrans (StateT s) where
    lift :: Monad m => m a -> StateT s m a
    lift ma = StateT $ \s -> do
        a <- ma
        return (a, s)

-- liftIO lifts to the IO monad, and can be a variable number of lifts, i.e. lift . lift
-- the monad we're using it in must be an instance of MonadIO for that to work, though
-- resides in Control.Monad.IO.Class


instance (MonadIO m, Monad (IdentityT m)) => MonadIO (IdentityT m) where
    liftIO = IdentityT . liftIO

instance (MonadIO m, Monad (EitherT e m)) => MonadIO (EitherT e m) where
    liftIO = lift . liftIO

instance (MonadIO m, Monad (MaybeT m)) => MonadIO (MaybeT m) where
    liftIO = lift . liftIO

instance (MonadIO m, Monad (ReaderT r m)) => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO

instance (MonadIO m, Monad (StateT r m)) => MonadIO (StateT r m) where
    liftIO = lift . liftIO

