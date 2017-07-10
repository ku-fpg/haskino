{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module:      Control.Remote.Monad,Types
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Monad.Types
  ( RemoteMonad(..)
  ) where


import           Control.Applicative
import           Control.Monad.Catch
import           Control.Remote.Applicative.Types

-- | 'RemoteMonad' is our monad that can be executed in a remote location.
data RemoteMonad  p a where
   Appl        :: RemoteApplicative p a -> RemoteMonad p a
   Bind        :: RemoteMonad p a -> (a -> RemoteMonad p b) -> RemoteMonad p b
   Ap'         :: RemoteMonad p (a -> b) -> RemoteMonad p a -> RemoteMonad p b
   Alt'        :: RemoteMonad p a -> RemoteMonad p a -> RemoteMonad p a
   Empty'      :: RemoteMonad p a
   Throw       :: Exception e => e -> RemoteMonad p a
   Catch       :: Exception e => RemoteMonad p a -> (e -> RemoteMonad p a)-> RemoteMonad p a

instance  Functor (RemoteMonad p) where
  fmap f m = pure f <*> m

instance  Applicative (RemoteMonad p) where
  pure a                = Appl (pure a)
  Appl f   <*> Appl g   = Appl (f <*> g)
  f        <*> g        = Ap' f g

instance Monad (RemoteMonad p) where
  return        = pure
  m >>= k       = Bind m k
  Empty' >> _m2 = Empty'
  m1 >> m2      = m1 *> m2 -- This improves our bundling opportunities

instance MonadThrow (RemoteMonad p) where
    throwM e = Throw e

instance MonadCatch (RemoteMonad p) where
    catch m f = Catch m f

instance Alternative (RemoteMonad p) where
    empty        = Empty'
    Empty' <|> p = p
    Appl g <|> Appl h = Appl (g <|> h)
    m1 <|> m2    = Alt' m1 m2
