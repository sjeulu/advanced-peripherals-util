module Control.Monad.Require where

import Flow ((|>))
import Control.Monad ((>=>))
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Identity
  ( Identity (runIdentity, Identity)
  , IdentityT (runIdentityT)
  )
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask))

--newtype RequireT i r m a = RequireT
--  { resolveT
--    :: forall t m'
--    . MonadTrans t
--    => Monad m'
--    => (i -> t m' (m r))
--    -> t m' (m a)
--  }
--
--type Require i r = RequireT i r Identity
--
--instance Monad m => Functor (RequireT i r m) where
--  fmap f (RequireT r) = RequireT $ fmap (fmap f) . r
--
--instance Monad m => Applicative (RequireT i r m) where
--  pure a = RequireT (pure $ pure $ pure a)
--  (RequireT f) <*> (RequireT a) = RequireT (liftA2 (liftA2 (<*>)) f a)
--
--instance Monad m => Monad (RequireT i r m) where
--  (RequireT m1) >>= k = RequireT
--    $ \h -> m1 h >>= \ma -> pure (resolveT (k <$> ma) h)
--
--instance MonadTrans (RequireT i r) where
--  lift m1 = RequireT $ const $ lift m1
--
--require :: i -> RequireT i r m r
--require i = RequireT ($ i)
--
--resolve :: Require i r a -> (i -> r) -> a
--resolve a h = resolveT a (lift . Identity . h)
--  |> runIdentityT
--  |> runIdentity
--

--data Require i r a where
--  Require :: i -> (r -> Require i r a) -> Require i r a
--  Pure :: a -> Require i r a
--
--instance Functor (Require i r) where
--  fmap f = \case
--    Pure a -> Pure $ f a
--    Require i k -> Require i $ fmap f . k
--
--instance Applicative (Require i r) where
--  pure = Pure
--  Pure f <*> a = f <$> a
--  Require i k <*> a = Require i \r -> k r <*> a
--
--instance Monad (Require i r) where
--  Pure a >>= k = k a
--  Require i k' >>= k = Require i (k' >=> k)
--
--newtype RequireT i r m a = RequireT
--  { runRequireT :: m (Require i r a)
--  }
--
--instance Functor m => Functor (RequireT i r m) where
--  fmap f (RequireT a) = RequireT $ fmap (fmap f) a
--
--instance Applicative m => Applicative (RequireT i r m) where
--  pure = RequireT . pure . pure
--  (RequireT f) <*> (RequireT a) = RequireT $ liftA2 (<*>) f a
--
--instance Monad m => Monad (RequireT i r m) where
--  (RequireT m1) >>= k = RequireT $ m1 >>= \m1' -> (m1' >>= k)
--
--instance MonadTrans (RequireT i r) where
--  lift m1 = RequireT $ fmap pure m1

newtype Require i r a = Require
  { runRequire :: forall m. Monad m => ReaderT
    (i -> m r)
    m
    a
  }


require :: i -> Require i r r
require i = Require $ ask >>= \h -> lift (h i)

resolve :: Monad m => (i -> m r) -> Require i r a -> m a
resolve h (Require m) = runReaderT m h


instance Functor (Require i r) where
  fmap f (Require a) = Require (f <$> a)

instance Applicative (Require i r) where
  pure a = Require $ pure a
  (Require f) <*> (Require a) = Require $ f <*> a

instance Monad (Require i r) where
  (Require m) >>= k = Require $ m >>= \a -> runRequire (k a)
