{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module:      Control.Remote.Applicative
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Applicative
  ( -- * The remote applicative
    RemoteApplicative
  , KnownResult(..)
    -- * The primitive lift functions
  , primitive
    -- * The run functions
  , RunApplicative(runApplicative)
  , runWeakApplicative
--  , runStrongApplicative
  , runApplicativeApplicative
  , runAlternativeApplicative
  ) where


import           Control.Monad.Trans.Class

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.Trans.Maybe
--import           Control.Monad.Trans.State
import           Control.Natural
import           Control.Remote.Applicative.Types  as T
import           Control.Remote.Packet.Alternative as Alt
import           Control.Remote.Packet.Applicative as A
import qualified Control.Remote.Packet.Query       as Q
--import           Control.Remote.Packet.Strong      (HStrongPacket (..),
--                                                    StrongPacket)
import qualified Control.Remote.Packet.Strong      as Strong
import           Control.Remote.Packet.Weak        (WeakPacket)
import qualified Control.Remote.Packet.Weak        as Weak
import           Control.Remote.Util               as U


-- | promote a command into the applicative
primitive :: p a -> RemoteApplicative p a
primitive prim = T.Primitive prim

-- | 'RunApplicative' is the overloading for choosing the appropriate bundling strategy for applicative.
class RunApplicative f where

  -- | This overloaded function chooses the appropriate bundling strategy
  --   based on the type of the handler your provide.
  runApplicative :: forall m prim . (MonadThrow m, KnownResult prim) => (f prim :~> m) -> (RemoteApplicative prim :~> m)


instance RunApplicative WeakPacket where
  runApplicative = runWeakApplicative

--instance RunApplicative StrongPacket where
--  runApplicative = runStrongApplicative

instance RunApplicative ApplicativePacket where
  runApplicative = runApplicativeApplicative

instance RunApplicative AlternativePacket where
  runApplicative = runAlternativeApplicative

instance RunApplicative Q.QueryPacket where
  runApplicative = runQueryApplicative

-- | The weak remote applicative, that sends commands and procedures piecemeal.
runWeakApplicative :: forall m prim. (MonadThrow m, KnownResult prim) => (WeakPacket prim :~> m) -> (RemoteApplicative prim :~> m)
runWeakApplicative (NT rf) = wrapNT go
  where
    go :: forall a . RemoteApplicative prim a ->  m a
    go p = do r <- runMaybeT (go2 p)
              case r of
                Nothing -> throwM RemoteEmptyException
                Just a  -> return a

    go2 :: forall a . RemoteApplicative prim a -> MaybeT m a
    go2 (T.Primitive prim) = lift $ rf (Weak.Primitive prim)
    go2 (T.Ap g h)         = go2 g <*> go2 h
    go2 (T.Pure      a)    = pure a
    go2 T.Empty            = empty
    go2 (T.Alt g h)        = go2 g <|> go2 h
{-
-- | The strong remote applicative, that bundles together commands.
runStrongApplicative :: forall m prim . (MonadThrow m, KnownResult prim) => (StrongPacket prim :~> m) -> (RemoteApplicative prim :~> m)
runStrongApplicative (NT rf) = wrapNT $ \ p -> do
    (r,HStrongPacket h) <- runStateT (runMaybeT (go p)) (HStrongPacket id)
    rf $ h $ Strong.Done
    case r of
      Just a -> return a
      Nothing -> throwM RemoteEmptyException
  where
    go :: RemoteApplicative prim a -> MaybeT (StateT (HStrongPacket prim) m) a
    go (T.Pure a)      = return a
    go (T.Primitive p) =
      case knownResult p of
        Just a -> lift $ do
                             () <-modify $ \ (HStrongPacket cs) -> HStrongPacket (cs  . Strong.Command  p)
                             return a

        Nothing ->  lift $ do
                              HStrongPacket cs <- get
                              put (HStrongPacket id)
                              r2 <- lift $ rf $ cs $ Strong.Procedure p
                              return $ r2
    go (T.Ap g h)      = go g <*> go h
    go (T.Alt g h)     = go g <|> go h
    go (T.Empty )      = empty
-}

-- | The applicative remote applicative, that is the identity function.
runApplicativeApplicative :: forall m prim . (MonadThrow m) => (ApplicativePacket prim :~> m) -> (RemoteApplicative prim :~> m)
runApplicativeApplicative (NT rf) = wrapNT (go4 . go3)
  where
    go3 :: forall a . RemoteApplicative prim a -> Wrapper (ApplicativePacket prim) a
    go3  T.Empty        = empty   --uses Throw'
    go3 (T.Pure a)      = pure a
    go3 (T.Primitive p) = Value (A.Primitive p)
    go3 (T.Ap g h)      = go3 g <*> go3 h
    go3 (T.Alt g h)     = go3 g <|> go3 h

    go4 :: forall a . Wrapper (ApplicativePacket prim) a -> m a
    go4 (Value pkt)  = rf pkt
    go4 (Throw' pkt) = do () <- rf pkt
                          throwM RemoteEmptyException

-- |
runQueryApplicative :: forall m q . (MonadThrow m) => (Q.QueryPacket q :~> m) -> (RemoteApplicative q :~> m)
runQueryApplicative (NT rf) = wrapNT (go4 . go3)
  where
    go3 :: forall a . RemoteApplicative q a -> Wrapper (Q.QueryPacket q) a
    go3  T.Empty    = empty   --uses Throw'
    go3 (T.Pure a)  = pure a
    go3 (T.Primitive q) = Value (Q.Primitive q)
    go3 (T.Ap g h)  = go3 g <*> go3 h
    go3 (T.Alt g h) = go3 g <|> go3 h

    go4 :: forall a . Wrapper (Q.QueryPacket q) a -> m a
    go4 (Value pkt)  = rf pkt
    go4 (Throw' pkt) = do () <- rf pkt
                          throwM RemoteEmptyException


runAlternativeApplicative :: forall m prim . (MonadThrow m) => (AlternativePacket prim :~> m) -> (RemoteApplicative prim :~> m)
runAlternativeApplicative (NT rf) = wrapNT $ \p ->  rf $ go p
   where
      go :: forall a . RemoteApplicative prim a -> AlternativePacket prim a
      go  T.Empty                = Alt.Empty
      go (T.Pure a)              = pure a
      go (T.Primitive p)         = Alt.Primitive p
      go (T.Ap g h)              = go g <*> go h
      go (T.Alt g h)             = go g <|> go h
