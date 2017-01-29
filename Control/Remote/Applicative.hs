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

module Control.Remote.Applicative
  ( -- * The remote applicative
    RemoteApplicative
    -- * The primitive lift functions
  , command
  , procedure
    -- * The run functions
  , RunApplicative(runApplicative)
  , runWeakApplicative
  , runStrongApplicative
  , runApplicativeApplicative
  , runAlternativeApplicative
  ) where


import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Identity
import Control.Category ((>>>))

import           Control.Remote.Monad.Packet.Applicative as A
import           Control.Remote.Monad.Packet.Alternative as Alt
import qualified Control.Remote.Monad.Packet.Strong as Strong
import           Control.Remote.Monad.Packet.Strong (StrongPacket, HStrongPacket(..))
import qualified Control.Remote.Monad.Packet.Weak as Weak
import           Control.Remote.Monad.Packet.Weak (WeakPacket)
import           Control.Remote.Monad.Types as T
import           Control.Natural
import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.Trans.Maybe


-- | promote a command into the applicative
command :: c -> RemoteApplicative c p ()
command c = T.Command c

-- | promote a command into the applicative
procedure :: p a -> RemoteApplicative  c p a
procedure p = T.Procedure p


-- | 'RunApplicative' is the overloading for choosing the appropriate bundling strategy for applicative.
class RunApplicative f where
  -- | This overloaded function chooses the appropriate bundling strategy
  --   based on the type of the handler your provide.
  runApplicative :: (MonadThrow m) => (f c p :~> m) -> (RemoteApplicative c p:~> m)

instance RunApplicative WeakPacket where
  runApplicative = runWeakApplicative

instance RunApplicative StrongPacket where
  runApplicative = runStrongApplicative

instance RunApplicative ApplicativePacket where
  runApplicative = runApplicativeApplicative

instance RunApplicative AlternativePacket where
  runApplicative = runAlternativeApplicative

-- | The weak remote applicative, that sends commands and procedures piecemeal.
runWeakApplicative :: forall m c p . (MonadThrow m) => (WeakPacket c p :~> m) -> (RemoteApplicative c p :~> m)
runWeakApplicative (NT rf) = wrapNT $ go
  where
    go :: forall a . RemoteApplicative c p a ->  m a
    go p = do r <- runMaybeT (go2 p)
              case r of
                Nothing -> throwM RemoteEmptyException
                Just a  -> return a

    go2 :: forall a . RemoteApplicative c p a -> MaybeT m a
    go2 (T.Command   c)  = lift $ rf (Weak.Command c)
    go2 (T.Procedure p)  = lift $ rf (Weak.Procedure p)
    go2 (T.Ap g h)      = go2 g <*> go2 h
    go2 (T.Pure      a) = pure a
    go2 T.Empty         = empty
    go2 (T.Alt g h)     = (go2 g <|> go2 h)

-- | The strong remote applicative, that bundles together commands.
runStrongApplicative :: forall m c p . (MonadThrow m) => (StrongPacket c p :~> m) -> (RemoteApplicative c p :~> m)
runStrongApplicative (NT rf) = wrapNT $ \ p -> do
    (r,HStrongPacket h) <- runStateT (runMaybeT (go p)) (HStrongPacket id)
    rf $ h $ Strong.Done
    case r of
      Just a -> return a
      Nothing -> throwM RemoteEmptyException
  where
    go :: forall a . RemoteApplicative c p a -> MaybeT (StateT (HStrongPacket c p) m) a
    go (T.Pure a)      = return a
    go (T.Command c)   = lift $ do
        modify (\ (HStrongPacket cs) -> HStrongPacket (cs . Strong.Command c))
        return ()
    go (T.Procedure p) = lift$ do
        HStrongPacket cs <- get
        put (HStrongPacket id)
        r2 <- lift $ rf $ cs $ Strong.Procedure $ p
        return $ r2
    go (T.Ap g h)      = go g <*> go h
    go (T.Alt g h)     = go g <|> go h
    go (T.Empty )      = empty


-- | The applicative remote applicative, that is the identity function.
runApplicativeApplicative :: forall m c p . (MonadThrow m) => (ApplicativePacket c p :~> m) -> (RemoteApplicative c p :~> m)
runApplicativeApplicative (NT rf) = wrapNT (go4 . go3)
  where
    go3 :: forall a . RemoteApplicative c p a -> Wrapper (ApplicativePacket c p) a
    go3 (T.Empty)       = empty   --uses Throw'
    go3 (T.Pure a)      = pure a
    go3 (T.Command c)   = Value (A.Command c)
    go3 (T.Procedure p) = Value (A.Procedure p)
    go3 (T.Ap g h)      = (go3 g) <*> (go3 h)
    go3 (T.Alt g h)     = (go3 g) <|> (go3 h)

    go4 :: forall a . Wrapper (ApplicativePacket c p) a -> m a
    go4 (Value pkt)  = rf pkt
    go4 (Throw' pkt) = do () <- rf pkt
                          throwM RemoteEmptyException



runAlternativeApplicative :: forall m c p . (MonadThrow m) => (AlternativePacket c p :~> m) -> (RemoteApplicative c p :~> m)
runAlternativeApplicative (NT rf) = wrapNT $ \p ->  rf $ go p
   where
      go :: forall a . RemoteApplicative c p a -> AlternativePacket c p a
      go (T.Empty)       = Alt.Empty
      go (T.Pure a)      = pure a
      go (T.Command c)   = Alt.Command c
      go (T.Procedure p) = Alt.Procedure p
      go (T.Ap g h)      = (go g) <*> (go h)
      go (T.Alt g h)     = (go g) <|> (go h)
