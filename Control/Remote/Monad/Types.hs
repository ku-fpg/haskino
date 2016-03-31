{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Control.Remote.Applicative
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Monad.Types 
  ( RemoteMonad(..)
  , RemoteApplicative(..)
  ) where


-- import qualified  Control.Remote.Monad.Packet.Applicative as A
import            Control.Natural

-- | 'RemoteMonad' is our monad that can be executed in a remote location.
data RemoteMonad c p a where
   Appl        :: RemoteApplicative c p a -> RemoteMonad c p a
   Bind        :: RemoteMonad c p a -> (a -> RemoteMonad c p b) -> RemoteMonad c p b
   Ap'         :: RemoteMonad c p (a -> b) -> RemoteMonad c p a -> RemoteMonad c p b
  
instance Functor (RemoteMonad c p) where
  fmap f m = pure f <*> m

instance Applicative (RemoteMonad c p) where
  pure a                = Appl (pure a)
  Appl f   <*> Appl g   = Appl (f <*> g)
  f        <*> g        = Ap' f g

{-
  Appl f   *> Appl g   = Appl (f *> g)
  Appl f   *> Bind m k = Bind (Appl f *> m) k
  Bind m k *> r        = Bind m (\ a -> k a *> r)

  Appl f   <* Appl g   = Appl (f <* g)

  Appl f   <* Bind m k = Bind (pure (,) <*> Appl f <*> m) (\ (a,b) -> pure a <* k b)
  Bind m k <* r        = Bind m (\ a -> k a <* r)
-}

instance Monad (RemoteMonad c p) where
  return = pure
  m >>= k    = Bind m k
  
  m1 >> m2 = m1 *> m2 -- This improves our bundling opportunities

-- | 'RemoteApplicative' is our applicative that can be executed in a remote location.
data RemoteApplicative c p a where 
   Command   :: c   -> RemoteApplicative c p () 
   Procedure :: p a -> RemoteApplicative c p a
   Ap        :: RemoteApplicative c p (a -> b) -> RemoteApplicative c p a -> RemoteApplicative c p b
   Pure      :: a                                                         -> RemoteApplicative c p a  

instance Functor (RemoteApplicative c p) where
  fmap f g = pure f <*> g

instance Applicative (RemoteApplicative c p) where
  pure a = Pure a
  (<*>) = Ap

