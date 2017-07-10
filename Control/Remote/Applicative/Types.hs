{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-|
Module:      Control.Remote.Applicative.Types
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Applicative.Types
  ( RemoteApplicative(..)
  , CP(..)
  ) where

import           Control.Applicative
import           Control.Remote.Util

data CP (c :: *) (p :: * -> *) a where
  Cmd   :: c   -> CP c p ()
  Proc  :: p a -> CP c p a

-- | 'RemoteApplicative' is our applicative that can be executed in a remote location.
data RemoteApplicative cp a where
   Primitive :: cp a -> RemoteApplicative cp a
   Alt       :: RemoteApplicative cp a -> RemoteApplicative cp a -> RemoteApplicative cp a
   Ap        :: RemoteApplicative cp (a -> b) -> RemoteApplicative cp a -> RemoteApplicative cp b
   Pure      :: a   -> RemoteApplicative cp a
   Empty     :: RemoteApplicative cp a

instance Functor (RemoteApplicative p) where
  fmap f g = pure f <*> g

instance Applicative (RemoteApplicative p) where
  pure  = Pure
  (<*>) = Ap

instance Alternative (RemoteApplicative p) where
   empty       = Empty
   Empty <|> p = p
   m1 <|> m2   = Alt m1 m2

instance KnownResult (CP c p) where
  knownResult (Cmd _)  = Just ()
  knownResult (Proc _) = Nothing

instance (KnownResult cp) => KnownResult (RemoteApplicative cp) where
  knownResult (Primitive p) = knownResult p
  knownResult (Pure a)      = pure a
  knownResult (Ap g h)      = knownResult g <*> knownResult h
  knownResult (Alt g h)     = knownResult g <|> knownResult h
  knownResult Empty         = Nothing

