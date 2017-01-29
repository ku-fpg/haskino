{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Control.Remote.Monad.Packet.Weak
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Monad
  ( -- * The remote monad
    RemoteMonad
  , RemoteMonadException(..)
    -- * The primitive lift functions
  , command
  , procedure
  , loop
    -- * The run functions
  , RunMonad(runMonad)
  , runWeakMonad
  , runStrongMonad
  , runApplicativeMonad
  , runAlternativeMonad
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import qualified Control.Remote.Applicative as A
import           Control.Remote.Monad.Packet.Applicative as A
import qualified Control.Remote.Monad.Packet.Alternative as Alt
import           Control.Remote.Monad.Packet.Weak as Weak
import           Control.Remote.Monad.Packet.Strong as Strong
import           Control.Remote.Monad.Types as T
import           Control.Applicative

import Control.Natural
import Control.Monad.Catch
import Control.Monad.Trans.Maybe


-- | promote a command into the remote monad
command :: c -> RemoteMonad c p ()
command = Appl . A.command

-- | promote a procedure into the remote monad
procedure :: p a -> RemoteMonad c p a
procedure = Appl . A.procedure

loop :: forall a c p l . (a-> Bool) -> RemoteMonad c p a -> RemoteMonad c p a
loop f m = do  res <- m
               if f res then
                 loop f m
                else
                  return res

-- | 'RunMonad' is the overloading for choosing the appropriate bundling strategy for a monad.
class RunMonad f where
  -- | This overloaded function chooses the appropriate bundling strategy
  --   based on the type of the handler your provide.
  runMonad :: (MonadCatch m) => (f c p :~> m) -> (RemoteMonad c p :~> m)

instance RunMonad WeakPacket where
  runMonad = runWeakMonad

instance RunMonad StrongPacket where
  runMonad = runStrongMonad

instance RunMonad ApplicativePacket where
  runMonad = runApplicativeMonad

instance RunMonad Alt.AlternativePacket where
  runMonad = runAlternativeMonad

-- | This is a remote monad combinator, that takes an implementation
--   of a remote applicative, splits the monad into applicatives
--   without any merge stragegy, and uses the remote applicative.
--   Every '>>=' will generate a call to the 'RemoteApplicative'
--   handler; as well as one terminating call.
--   Using 'runBindeeMonad' with a 'runWeakApplicative' gives the weakest remote monad.
runMonadSkeleton :: (MonadCatch m) => (RemoteApplicative c p :~> m) -> (RemoteMonad c p :~> m)
runMonadSkeleton f = wrapNT $ \ case
  Appl g   -> unwrapNT f g
  Bind g k -> (runMonadSkeleton f # g) >>= \ a -> runMonadSkeleton f # (k a)
  Ap' g h  -> (runMonadSkeleton f # g) <*> (runMonadSkeleton f # h)
  Alt' m1 m2 -> (runMonadSkeleton f # m1)
                  `catch`(\ e-> case e :: RemoteMonadException of
                            RemoteEmptyException -> runMonadSkeleton f # m2
                         )
  Empty'    -> throwM RemoteEmptyException
  Throw e   -> throwM e
  Catch m h -> catch (runMonadSkeleton f # m)  ((runMonadSkeleton f #) . h)

-- | This is the classic weak remote monad, or technically the
--   weak remote applicative weak remote monad.
runWeakMonad :: (MonadCatch m) => (WeakPacket c p :~> m) -> (RemoteMonad c p :~> m)
runWeakMonad = runMonadSkeleton . A.runWeakApplicative


-- | This is the classic strong remote monad. It bundles
--   packets (of type 'StrongPacket') as large as possible,
--   including over some monadic binds.
runStrongMonad :: forall m c p . (MonadCatch m) => (StrongPacket c p :~> m) -> (RemoteMonad c p :~> m)
runStrongMonad (NT rf) = wrapNT $ \ p -> do
    (r,HStrongPacket h) <- runStateT (runMaybeT (go2 p)) (HStrongPacket id)
    rf $ h $ Strong.Done
    case r of
       Nothing -> throwM RemoteEmptyException
       Just v  -> return v
  where
    go2 :: forall a . RemoteMonad c p a -> MaybeT (StateT (HStrongPacket c p) m) a
    go2 (Appl app)   = go app
    go2 (Bind app k) = go2 app >>= \ a -> go2 (k a)
    go2 (Ap' g h)    = go2 g <*> go2 h
    go2 (Alt' m1 m2) = go2 m1  <|> go2 m2
    go2 Empty'       = empty
    go2 (Throw e)    = lift $ do
        HStrongPacket cs <-  get
        put (HStrongPacket id)
        () <- lift $ rf $ cs Strong.Done
        throwM e
    go2 (Catch m h) = catch (go2 m) (go2 . h)

    go :: forall a . RemoteApplicative c p a -> MaybeT (StateT (HStrongPacket c p) m) a
    go (T.Pure a)      = return a
    go (T.Command c)   = lift $ do
        modify (\ (HStrongPacket cs) -> HStrongPacket (cs . Strong.Command c))
        return ()
    go (T.Procedure p) = lift $ do
        HStrongPacket cs <- get
        put (HStrongPacket id)
        r2 <- lift $ rf $ cs $ Strong.Procedure $ p
        return $ r2
    go (T.Ap g h) = go g <*> go h
    go (T.Alt g h) = go g <|> go h

-- | The is the strong applicative strong remote monad. It bundles
--   packets (of type 'RemoteApplicative') as large as possible,
--   including over some monadic binds.
runApplicativeMonad :: forall m c p . (MonadCatch m) => (A.ApplicativePacket c p :~> m) -> (RemoteMonad c p :~> m)
runApplicativeMonad (NT rf) = wrapNT $ \ p -> do
    (r,h) <-  runStateT (runMaybeT (go2 p)) (pure ())
    case  pk h of -- should we stub out the call with only 'Pure'?
      Pure' a ->  return a
      Pkt f b ->  do res <- rf $ b
                     return $ f res
    case r of
      Nothing -> throwM RemoteEmptyException
      Just v -> return v
  where
    go2 :: forall a . RemoteMonad c p a -> MaybeT (StateT (RemoteApplicative c p ()) m) a
    go2 (Appl app)   = lift $ unwrap $ go app
    go2 (Bind app k) = go2 app >>= \ a -> go2 (k a)
    go2 (Ap' g h)    = go2 g <*> go2 h
    go2 (Alt' m1 m2) = go2 m1 <|> go2 m2
    go2 Empty'       = empty
    go2 (Throw e)    = lift $ do
        ()<-discharge id
        throwM e
    go2 (Catch m h) = catch (go2 m) (go2 . h)

    go :: forall a . T.RemoteApplicative c p a -> Wrapper (RemoteApplicative c p) a
    go (T.Empty) = empty
    go (T.Pure a) = pure a
    go (T.Command c) = Value (T.Command c)
    go (T.Procedure p) = Value (T.Procedure p)
    go (T.Ap g h)      = (go g) <*> (go h)
    go (T.Alt g h)     = (go g) <|> (go h)

    -- g is a function that will take the current state as input
    discharge :: forall a f . Applicative f => (f () ->RemoteApplicative c p a )-> StateT (f ()) m a
    discharge g = do
                 ap' <- get
                 put (pure ()) -- clear state
                 case pk $ g ap' of
                    Pure' a -> return a
                    Pkt f pkt -> do
                                    res <- lift $ rf pkt
                                    return $ f res
    -- Given a wrapped applicative discharge via local monad
    unwrap :: forall a . Wrapper(RemoteApplicative c p) a -> StateT (RemoteApplicative c p ()) m a
    unwrap (Value ap) = case superApplicative ap of
                            Nothing ->do
                                      discharge $ \ap' -> (ap' *> ap)
                            Just a  ->do
                                       modify (\ap' -> ap' <* ap)
                                       return a

    unwrap (Throw' ap) = do
                         discharge $ \ap' -> (ap' <* ap)
                         throwM RemoteEmptyException

    -- Do we know the answer? Nothing  =  we need to get it
    superApplicative :: RemoteApplicative c p a -> Maybe a
    superApplicative (T.Pure a)      = pure a
    superApplicative (T.Command   c) = Just ()
    superApplicative (T.Procedure p) = Nothing
    superApplicative (T.Ap g h)      =  (superApplicative g) <*> (superApplicative h)
    superApplicative (T.Alt g h)     = Nothing
    superApplicative (T.Empty)       = Nothing

    -- Either A or a Packet to return A
    pk :: RemoteApplicative c p a -> X c p a
    pk (T.Pure a)      = Pure' a
    pk (T.Command   c) = Pkt id $ A.Command c
    pk (T.Procedure p) = Pkt id $ A.Procedure p
    pk (T.Ap g h)      = case (pk g, pk h) of
                           (Pure' a, Pure' b)   -> Pure' (a b)
                           (Pure' a, Pkt f b)   -> Pkt (\b' -> a (f b')) b
                           (Pkt f a, Pure' b)   -> Pkt (\a' -> f a' b) a
                           (Pkt f a, Pkt g b)   -> Pkt id $ A.Zip (\ a' b' -> f a' (g b')) a b
data X c p a where
   Pure' :: a -> X c p a
   Pkt  :: (a -> b) -> ApplicativePacket c p a -> X c p b

runAlternativeMonad :: forall m c p . (MonadCatch m) => (Alt.AlternativePacket c p :~> m) -> (RemoteMonad c p :~> m)
runAlternativeMonad (NT rf) = wrapNT $ \ p -> do
   (r,h) <-  runStateT (runMaybeT (go2 p)) (pure ())
   () <- rf $ pk h
   case r of
      Nothing -> throwM RemoteEmptyException
      Just v -> return v

   where
    go2 :: forall a . RemoteMonad c p a -> MaybeT (StateT (RemoteApplicative  c p ()) m) a
    go2 (Appl app)   = lift $ go app
    go2 (Bind app k) = go2 app >>= \ a -> go2 (k a)
    go2 (Ap' g h)    = go2 g <*> go2 h
    go2 (Alt' m1 m2) = go2 m1 <|> go2 m2
    go2 Empty'       = empty
    go2 (Throw e)    = lift $ do
        ()<- discharge id
        throwM e
    go2 (Catch m h) = catch (go2 m) (go2 . h)

    go :: RemoteApplicative c p a -> StateT (RemoteApplicative c p ()) m a
    go ap = case superApplicative ap of
               Nothing -> do
                           discharge $ \ ap' -> ap' *> ap
               Just a  -> do
                             modify (\ap' -> ap' <* ap)
                             return a

    pk :: forall a . RemoteApplicative c p a -> Alt.AlternativePacket c p a
    pk (T.Empty)       = empty
    pk (T.Pure a)      = pure a
    pk (T.Command c)   = (Alt.Command c)
    pk (T.Procedure p) = (Alt.Procedure p)
    pk (T.Ap g h)      = (pk g) <*> (pk h)
    pk (T.Alt g h)     = (pk g) <|> (pk h)

    -- g is a function that will take the current state as input
    discharge :: forall a f . Applicative f => (f () ->RemoteApplicative c p a )-> StateT (f ()) m a
    discharge g = do
                 ap' <- get
                 put (pure ()) -- clear state
                 lift $ rf $ pk $ g ap'

    superApplicative :: RemoteApplicative c p a -> Maybe a
    superApplicative (T.Empty)       = Nothing
    superApplicative (T.Pure a)      = pure a
    superApplicative (T.Command c)   = pure ()
    superApplicative (T.Procedure p) = Nothing
    superApplicative (T.Ap g h)      = (superApplicative g) <*> (superApplicative h)
    superApplicative (T.Alt g h)      = (superApplicative g) <|> (superApplicative h)



