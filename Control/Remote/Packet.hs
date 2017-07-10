{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-|
Module:      Control.Remote.Monad.Packet
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Packet
  (
    Promote(..)
  , promoteToStrong
  , promoteToApplicative
  , promoteToAlternative
  , promoteToQuery
  ) where
import           Control.Applicative
import           Control.Natural
import qualified Control.Remote.Packet.Alternative as Alt
import qualified Control.Remote.Packet.Applicative as A
import qualified Control.Remote.Packet.Query       as Q
import qualified Control.Remote.Packet.Strong      as Strong
import qualified Control.Remote.Packet.Weak        as Weak

class Promote f where
    promote :: (Applicative m) => (Weak.WeakPacket p :~> m) -> (f p :~> m)

instance Promote A.ApplicativePacket where
   promote =  promoteToApplicative

-- | promotes a function that can work over WeakPackets to a function that can work over Alternative Packets
promoteToAlternative :: forall p m . (Alternative m) => (Weak.WeakPacket p :~> m) -> (Alt.AlternativePacket p :~> m)
promoteToAlternative (NT f) =  NT alternativeFunc
                   where
                        alternativeFunc :: (Alternative m) => (Alt.AlternativePacket p a -> m a)
                        alternativeFunc (Alt.Primitive p) = f (Weak.Primitive p)
                        alternativeFunc (Alt.Zip f1 a b)  = f1 <$> alternativeFunc a <*> alternativeFunc b
                        alternativeFunc (Alt.Alt a b)     = alternativeFunc a <|> alternativeFunc b
                        alternativeFunc (Alt.Pure a)      = pure a
                        alternativeFunc  Alt.Empty        = empty


-- | promotes a function that can work over WeakPackets to a function that can work over Applicative Packets
promoteToApplicative :: forall p m . (Applicative m) => (Weak.WeakPacket p :~> m) -> (A.ApplicativePacket p :~> m)
promoteToApplicative (NT f) =  NT applicativeFunc
                    where
                        applicativeFunc :: (Applicative m) => (A.ApplicativePacket p a -> m a)
                        applicativeFunc (A.Primitive p) = f (Weak.Primitive p)
                        applicativeFunc (A.Zip f1 a b)  = f1 <$> applicativeFunc a <*> applicativeFunc b
                        applicativeFunc (A.Pure a)      = pure a

-- | promotes a function that can work over WeakPackets to a function that can work over Query Packets
promoteToQuery :: forall p m . (Applicative m) => (Weak.WeakPacket p :~> m) -> (Q.QueryPacket p :~> m)
promoteToQuery (NT f) =  NT queryFunc
                   where
                        queryFunc :: (Applicative m) => (Q.QueryPacket p a -> m a)
                        queryFunc (Q.Primitive p) = f (Weak.Primitive p)
                        queryFunc (Q.Zip f1 a b)  = f1 <$> queryFunc a <*> queryFunc b
                        queryFunc (Q.Pure a)      = pure a


-- | promotes a function that can work over WeakPackets to a function that can work over Strong Packets
promoteToStrong :: forall p m . (Applicative m) => (Weak.WeakPacket p :~> m) -> (Strong.StrongPacket p :~> m)
promoteToStrong (NT f) = NT $ strongFunc
                       where
                           strongFunc :: (Applicative m) => (Strong.StrongPacket p a -> m a)
                           strongFunc (Strong.Command c cmds) = f (Weak.Primitive c) *> strongFunc cmds
                           strongFunc (Strong.Procedure p)    = f (Weak.Primitive p)
                           strongFunc (Strong.Done)           = pure ()

