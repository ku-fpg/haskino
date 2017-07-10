{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

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
  , KnownResult(..)
    -- * The primitive lift functions
  , primitive
  , loop
    -- * The run functions
  , RunMonad(runMonad)
  , runWeakMonad
 -- , runStrongMonad
  , runApplicativeMonad
  , runQueryMonad
  , runAlternativeMonad
  ) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict

import qualified Control.Remote.Applicative        as A
import           Control.Remote.Applicative.Types  as AT
import           Control.Remote.Monad.Types        as T
import qualified Control.Remote.Packet.Alternative as Alt
import           Control.Remote.Packet.Applicative as A
import           Control.Remote.Packet.Query       as Q
--import           Control.Remote.Packet.Strong      as Strong
import           Control.Remote.Packet.Weak        as Weak
import           Control.Remote.Util

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.Trans.Maybe
import           Control.Natural


primitive :: p a -> RemoteMonad p a
primitive = T.Appl . AT.Primitive

loop :: forall a p . (a-> Bool) -> RemoteMonad p a -> RemoteMonad p a
loop f m = do  res <- m
               if f res then
                 loop f m
                else
                  return res

data X (f :: ( * -> *) -> * -> *) p a where
  Pure' :: a -> X f p a
  Pkt  :: (a -> b) -> f p a -> X f p b


-- | 'RunMonad' is the overloading for choosing the appropriate bundling strategy for a monad.
class RunMonad f where
  -- | This overloaded function chooses the appropriate bundling strategy
  --   based on the type of the handler your provide.
  runMonad :: (MonadCatch m, KnownResult prim) => (f prim :~> m) -> (RemoteMonad prim :~> m)

instance RunMonad WeakPacket where
  runMonad = runWeakMonad

--instance RunMonad StrongPacket where
--  runMonad = runStrongMonad

instance RunMonad ApplicativePacket where
  runMonad = runApplicativeMonad

instance RunMonad Alt.AlternativePacket where
  runMonad = runAlternativeMonad

instance RunMonad Q.QueryPacket where
  runMonad = runQueryMonad

-- | This is a remote monad combinator, that takes an implementation
--   of a remote applicative, splits the monad into applicatives
--   without any merge stragegy, and uses the remote applicative.
--   Every '>>=' will generate a call to the 'RemoteApplicative'
--   handler; as well as one terminating call.
--   Using 'runBindeeMonad' with a 'runWeakApplicative' gives the weakest remote monad.
runMonadSkeleton :: (MonadCatch m, KnownResult prim) => (RemoteApplicative prim :~> m) -> (RemoteMonad prim :~> m)
runMonadSkeleton f = wrapNT $ \ case
  Appl g   -> unwrapNT f g
  Bind g k -> (runMonadSkeleton f # g) >>= \ a -> runMonadSkeleton f # k a
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
runWeakMonad :: (MonadCatch m, KnownResult prim) => (WeakPacket prim :~> m) -> (RemoteMonad prim :~> m)
runWeakMonad = runMonadSkeleton . A.runWeakApplicative

{-
-- | This is the classic strong remote monad. It bundles
--   packets (of type 'StrongPacket') as large as possible,
--   including over some monadic binds.
runStrongMonad :: forall m prim . (MonadCatch m, KnownResult prim) => (StrongPacket prim :~> m) -> (RemoteMonad prim :~> m)
runStrongMonad (NT rf) = wrapNT $ \ p -> do
    (r,HStrongPacket h) <- runStateT (runMaybeT (go2 p)) (HStrongPacket id)
    rf $ h $ Strong.Done
    case r of
       Nothing -> throwM RemoteEmptyException
       Just v  -> return v
  where
    go2 :: forall a . RemoteMonad prim a -> MaybeT (StateT (HStrongPacket prim) m) a
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

    go :: forall a . (KnownResult prim) =>  RemoteApplicative prim a -> MaybeT (StateT (HStrongPacket prim) m) a
    go (AT.Pure a)      = return a
    go (AT.Primitive p) =
      case knownResult p of
        Just a -> lift $ do
          modify $ (\ (HStrongPacket cs) -> HStrongPacket (cs (Strong.Primitive p) ))
          return a
        Nothing ->  lift $ do
          HStrongPacket cs <- get
          put (HStrongPacket id)
          r2 <- lift $ rf $ cs $ Strong.Procedure p

          return $ r2

    go (AT.Ap g h) = go g <*> go h
    go (AT.Alt g h) = go g <|> go h

-}
type PreProcessor q = RemoteMonad q :~> RemoteMonad q

runApplicativeBase :: forall f m prim . (MonadCatch m, KnownResult prim) => (f prim :~> m)
               -> (RemoteApplicative prim :~> X f prim)
               -> PreProcessor prim -> (RemoteMonad prim :~> m)
runApplicativeBase (NT rf) (NT pk) (NT reWrite) = wrapNT $ \ q -> do
    (r,h) <-  runStateT (runMaybeT (go2 (reWrite q))) (pure ())
    case  pk h of -- should we stub out the call with only 'Pure'?
      Pure' a ->  return a
      Pkt f b ->  do res <- rf b
                     return $ f res
    case r of
      Nothing -> throwM RemoteEmptyException
      Just v -> return v
  where
    go2 :: forall a . RemoteMonad prim a -> MaybeT (StateT (RemoteApplicative prim ()) m) a
    go2 (Appl app)   = lift $ unwrap $ go app
    go2 (Bind app k) = go2 app >>= \ a -> go2 (k a)
    go2 (Ap' g h)    = go2 g <*> go2 h
    go2 (Alt' m1 m2) = go2 m1 <|> go2 m2
    go2 Empty'       = empty
    go2 (Throw e)    = lift $ do
        ()<-discharge id
        throwM e
    go2 (Catch m h) = catch (go2 m) (go2 . h)

    go :: forall a . RemoteApplicative prim a -> Wrapper (RemoteApplicative prim) a
    go  AT.Empty    = empty
    go (AT.Pure a)  = pure a
    go (AT.Primitive q) = Value $ AT.Primitive q
    go (AT.Ap g h)  = go g <*> go h
    go (AT.Alt g h) = go g <|> go h

    -- g is a function that will take the current state as input
    discharge :: forall a h . Applicative h => (h () ->RemoteApplicative prim a )-> StateT (h ()) m a
    discharge g = do
                 ap' <- get
                 put (pure ()) -- clear state
                 case pk $ g ap' of
                    Pure' a -> return a
                    Pkt f pkt -> do
                                    res <- lift $ rf pkt
                                    return $ f res
    -- Given a wrapped applicative discharge via local monad
    unwrap :: forall a . Wrapper(RemoteApplicative prim) a -> StateT (RemoteApplicative prim ()) m a
    unwrap (Value ap) = case knownResult ap of
                            Nothing -> discharge $ \ap' -> ap' *> ap
                            Just a  ->do
                                       modify $ \ap' -> ap' <* ap
                                       return a

    unwrap (Throw' ap) = do
                         discharge $ \ap' -> ap' <* ap
                         throwM RemoteEmptyException

-- | The is the strong applicative remote monad. It bundles
--   packets (of type 'RemoteApplicative') as large as possible,
--   including over some monadic binds.
runApplicativeMonad :: forall m prim . (MonadCatch m, KnownResult prim) => (A.ApplicativePacket prim :~> m) -> (RemoteMonad prim :~> m)
runApplicativeMonad f = runApplicativeBase f (wrapNT pk) (wrapNT id)
  where
    -- Either A or a Packet to return A
    pk :: RemoteApplicative prim a -> X ApplicativePacket prim a
    pk (AT.Pure a)      = Pure' a
    pk (AT.Primitive p) = Pkt id $ A.Primitive p
    pk  AT.Empty        = error "Empty shouldn't exist at packetization stage for ApplicativePacket"
    pk (AT.Alt _ _)     = error "'<|>' shouldn't exist at packetization stage for ApplicativePacket"
    pk (AT.Ap g h)      = case (pk g, pk h) of
                           (Pure' a, Pure' b)   -> Pure' (a b)
                           (Pure' a, Pkt j b)   -> Pkt (a . j) b
                           (Pkt j a, Pure' b)   -> Pkt (`j` b) a
                           (Pkt j a, Pkt k b)   -> Pkt id $ A.Zip (\ a' b' -> j a' (k b')) a b

-- |
runQueryMonad :: forall m prim . (MonadCatch m, KnownResult prim) => (Q.QueryPacket prim :~> m) -> (RemoteMonad prim :~> m)
runQueryMonad f = runApplicativeBase f (wrapNT pk) (wrapNT helper)
  where
    -- Either A or a Packet to return A
    pk :: RemoteApplicative prim a -> X QueryPacket prim a
    pk (AT.Pure a)  = Pure' a
    pk (AT.Primitive p) = Pkt id $ Q.Primitive p
    pk (AT.Alt _ _)     = error "'<|>' shouldn't exist at packetization stage for QueryPacket"
    pk  AT.Empty        = error "Empty shouldn't exist at packetization stage for QueryPacket"
    pk (AT.Ap g h)  = case (pk g, pk h) of
                       (Pure' a, Pure' b)   -> Pure' (a b)
                       (Pure' a, Pkt j b)   -> Pkt (a . j ) b
                       (Pkt j a, Pure' b)   -> Pkt (`j` b ) a
                       (Pkt j a, Pkt k b) -> Pkt id $ Q.Zip (\ a' b' -> j a' (k b')) a b

    helper:: RemoteMonad prim a -> RemoteMonad prim a
    helper (Ap' x@(Ap' _ _) y@(Ap' _ _))    = helper x <*> helper y
    helper (Ap' (Bind m1 k1) (Bind m2 k2) ) = liftA2 (,)  (helper m1) (helper m2) >>=
                                                  \(x1,x2) ->helper ( k1 x1) <*> helper (k2 x2)
    helper (Ap' (Bind m1 k1) app)           = liftA2 (,) (helper m1) (helper app) >>=
                                                  \(x1,x2) -> helper (k1 x1) <*> pure x2
    helper (Ap' (Ap' app (Bind m1 k1))   (Bind m2 k2))  =
      liftA3 (,,) (helper app) (helper m1) (helper  m2) >>=
          \(x1,x2,x3) -> (pure x1 <*> k1 x2) <*> helper (k2 x3)
    helper (Bind m k) =  helper m >>= \ x -> helper (k x)
    helper x = x

runAlternativeMonad :: forall m prim . (MonadCatch m, KnownResult prim) => (Alt.AlternativePacket prim :~> m) -> (RemoteMonad prim :~> m)
runAlternativeMonad (NT rf) = wrapNT $ \ p -> do
   (r,h) <-  runStateT (runMaybeT (go2 p)) (pure ())
   () <- rf $ pk h
   case r of
      Nothing -> throwM RemoteEmptyException
      Just v -> return v

   where
    go2 :: forall a . RemoteMonad prim a -> MaybeT (StateT (RemoteApplicative prim ()) m) a
    go2 (Appl app)   = lift $ go app
    go2 (Bind app k) = go2 app >>= \ a -> go2 (k a)
    go2 (Ap' g h)    = go2 g <*> go2 h
    go2 (Alt' m1 m2) = go2 m1 <|> go2 m2
    go2 Empty'       = empty
    go2 (Throw e)    = lift $ do
        ()<- discharge id
        throwM e
    go2 (Catch m h) = catch (go2 m) (go2 . h)

    go :: forall a .  RemoteApplicative prim a -> StateT (RemoteApplicative prim ()) m a
    go ap = case knownResult ap of
               Nothing -> discharge $ \ ap' -> ap' *> ap
               Just a  -> do
                          modify $ \ap' -> ap' <* ap
                          return a

    pk :: forall a . RemoteApplicative prim a -> Alt.AlternativePacket prim a
    pk AT.Empty                       = empty
    pk (AT.Pure a)                    = pure a
    pk (AT.Primitive p)               = Alt.Primitive p
    pk (AT.Ap g h)                    = pk g <*> pk h
    pk (AT.Alt g h)                   = pk g <|> pk h

    -- g is a function that will take the current state as input
    discharge :: forall a f . Applicative f => (f () ->RemoteApplicative prim a )-> StateT (f ()) m a
    discharge g = do
                 ap' <- get
                 put (pure ()) -- clear state
                 lift $ rf $ pk $ g ap'

