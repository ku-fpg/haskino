{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-|
Module:      Control.Remote.Monad.Packet
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Monad.Packet
  (
    Promote(..)
  , promoteToStrong
  , promoteToApplicative
  , promoteToAlternative
  ) where
import qualified Control.Remote.Monad.Packet.Weak as Weak
import qualified Control.Remote.Monad.Packet.Strong as Strong
import qualified Control.Remote.Monad.Packet.Applicative as A
import qualified Control.Remote.Monad.Packet.Alternative as Alt
import           Control.Natural
import           Control.Applicative

class Promote f where
    promote :: (Applicative m) => (Weak.WeakPacket c p :~> m) -> (f c p :~> m)

instance Promote A.ApplicativePacket where
   promote f =  promoteToApplicative f

instance Promote Strong.StrongPacket where
   promote f = promoteToStrong f


-- | promotes a function that can work over WeakPackets to a function that can work over Alternative Packets
promoteToAlternative :: forall c p m . (Alternative m) => (Weak.WeakPacket c p :~> m) -> (Alt.AlternativePacket c p :~> m)
promoteToAlternative (NT f) =  NT $ alternativeFunc
                   where
                        alternativeFunc :: (Alternative m) => (Alt.AlternativePacket c p a -> m a)
                        alternativeFunc (Alt.Command c) =  f (Weak.Command c)
                        alternativeFunc (Alt.Procedure p) = f (Weak.Procedure p)
                        alternativeFunc (Alt.Zip f1 a b) =  f1 <$> alternativeFunc a <*> alternativeFunc b
                        alternativeFunc (Alt.Alt a b) =  alternativeFunc a <|> alternativeFunc b
                        alternativeFunc (Alt.Pure a) = pure a


-- | promotes a function that can work over WeakPackets to a function that can work over Applicative Packets
promoteToApplicative :: forall c p m . (Applicative m) => (Weak.WeakPacket c p :~> m) -> (A.ApplicativePacket c p :~> m)
promoteToApplicative (NT f) =  NT $ applicativeFunc
                    where
                        applicativeFunc :: (Applicative m) => (A.ApplicativePacket c p a -> m a)
                        applicativeFunc (A.Command c) =  f (Weak.Command c)
                        applicativeFunc (A.Procedure p) = f (Weak.Procedure p)
                        applicativeFunc (A.Zip f1 a b) =  f1 <$> applicativeFunc a <*> applicativeFunc b
                        applicativeFunc (A.Pure a) = pure a


-- | promotes a function that can work over WeakPackets to a function that can work over Strong Packets
promoteToStrong :: forall c p m . (Applicative m) => (Weak.WeakPacket c p :~> m) -> (Strong.StrongPacket c p :~> m)
promoteToStrong (NT f) = NT $ strongFunc
                       where
                           strongFunc :: (Applicative m) => (Strong.StrongPacket c p a -> m a)
                           strongFunc (Strong.Command c cmds) = f (Weak.Command c) *> strongFunc cmds
                           strongFunc (Strong.Procedure p)    = f (Weak.Procedure p)
                           strongFunc (Strong.Done)           = pure ()
