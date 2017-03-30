module OuterInner where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
{-

newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a)) }

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

-}

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap :: () -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap


-- base monad in a monadtrans stack is the OUTERMOST monad
