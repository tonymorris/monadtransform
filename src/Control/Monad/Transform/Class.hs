{-# LANGUAGE Rank2Types #-}

module Control.Monad.Transform.Class(
  BindTransform(..)
, MonadTransform(..)
) where

import Data.Functor.Bind
import Data.Functor.Identity
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Control.Monad.Trans.RWS
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.List
import Control.Monad.Trans.Error

class BindTransform t where
  transformB ::
    (Bind f, Bind g) =>
    (forall z. f z -> g z)
    -> t f a
    -> t g a

instance BindTransform IdentityT where
  transformB f (IdentityT k) =
    IdentityT (f k)

instance BindTransform (ReaderT r) where
  transformB f (ReaderT k) =
    ReaderT (f . k)

instance BindTransform (WriterT w) where
  transformB f (WriterT k) =
    WriterT (f k)

instance BindTransform (StateT s) where
  transformB f (StateT k) =
    StateT (f . k)

instance BindTransform (RWST r w s) where
  transformB f (RWST k) =
    RWST (\r -> f . k r)

instance BindTransform MaybeT where
  transformB f (MaybeT k) =
    MaybeT (f k)

instance BindTransform ListT where
  transformB f (ListT k) =
    ListT (f k)

instance BindTransform (ErrorT e) where
  transformB f (ErrorT k) =
    ErrorT (f k)

class BindTransform t => MonadTransform t where
  transform ::
    (Monad f, Monad g) =>
    (forall z. f z -> g z)
    -> t f a
    -> t g a

  liftHom ::
    Monad g =>
    t Identity a
    -> t g a
  liftHom =
    transform (return . runIdentity)

instance MonadTransform IdentityT where
  transform f (IdentityT k) =
    IdentityT (f k)

instance MonadTransform (ReaderT r) where
  transform f (ReaderT k) =
    ReaderT (f . k)

instance MonadTransform (WriterT w) where
  transform f (WriterT k) =
    WriterT (f k)

instance MonadTransform (StateT s) where
  transform f (StateT k) =
    StateT (f . k)

instance MonadTransform (RWST r w s) where
  transform f (RWST k) =
    RWST (\r -> f . k r)

instance MonadTransform MaybeT where
  transform f (MaybeT k) =
    MaybeT (f k)

instance MonadTransform ListT where
  transform f (ListT k) =
    ListT (f k)

instance MonadTransform (ErrorT e) where
  transform f (ErrorT k) =
    ErrorT (f k)
