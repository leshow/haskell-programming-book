{-# LANGUAGE InstanceSigs #-}
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
-- liftIO lifts to the IO monad, and can be a variable number of lifts, i.e. lift . lift
-- the monad we're using it in must be an instance of MonadIO for that to work, though

instance MonadTrans (StateT s) where
    lift :: Monad m => m a -> StateT s m a
    lift ma = StateT $ \s -> do
        a <- ma
        return (a, s)
