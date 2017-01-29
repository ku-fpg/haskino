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
  , RemoteMonadException(..)
  , Wrapper(..)
  ) where


import            Control.Natural
import            Control.Monad.Catch
import            Control.Applicative
import            Data.Typeable
import            Control.Monad.Trans.Class

-- | 'RemoteMonad' is our monad that can be executed in a remote location.
data RemoteMonad  (cmd:: *) (proc:: * -> *) a where
   Appl        :: RemoteApplicative cmd proc a -> RemoteMonad cmd proc a
   Bind        :: RemoteMonad cmd proc a -> (a -> RemoteMonad cmd proc b) -> RemoteMonad cmd proc b
   Ap'         :: RemoteMonad cmd proc (a -> b) -> RemoteMonad cmd proc a -> RemoteMonad cmd proc b
   Alt'        :: RemoteMonad cmd proc a -> RemoteMonad cmd proc a -> RemoteMonad cmd proc a
   Empty'      :: RemoteMonad cmd proc a 
   Throw       :: Exception e => e -> RemoteMonad cmd proc a
   Catch       :: Exception e => RemoteMonad cmd proc a -> (e -> RemoteMonad cmd proc a)-> RemoteMonad cmd proc a
  
instance  Functor (RemoteMonad cmd proc) where
  fmap f m = pure f <*> m

instance  Applicative (RemoteMonad cmd proc) where
  pure a                = Appl (pure a)
  Appl f   <*> Appl g   = Appl (f <*> g)
  f        <*> g        = Ap' f g

instance Monad (RemoteMonad cmd proc) where
  return      = pure
  m >>= k     = Bind m k
  Empty' >> m2 = Empty'
  m1 >> m2    = m1 *> m2 -- This improves our bundling opportunities

instance MonadThrow (RemoteMonad cmd proc) where
    throwM e = Throw e

instance MonadCatch (RemoteMonad cmd proc) where
    catch m f = Catch m f

instance Alternative (RemoteMonad cmd proc) where
    empty        = Empty'
    Empty' <|> p = p
    Appl g <|> Appl h = Appl (g <|> h)
    m1 <|> m2    = Alt' m1 m2

-- | 'RemoteApplicative' is our applicative that can be executed in a remote location.
data RemoteApplicative (cmd:: *) (proc:: * -> *) a where 
   Command   :: cmd   -> RemoteApplicative cmd proc () 
   Procedure :: proc a -> RemoteApplicative cmd proc a
   Alt       :: RemoteApplicative cmd proc a -> RemoteApplicative cmd proc a -> RemoteApplicative cmd proc a
   Ap        :: RemoteApplicative cmd proc (a -> b) -> RemoteApplicative cmd proc a -> RemoteApplicative cmd proc b
   Pure      :: a   -> RemoteApplicative cmd proc a  
   Empty     :: RemoteApplicative cmd proc a
  
instance Functor (RemoteApplicative cmd proc) where
  fmap f g = pure f <*> g

instance Applicative (RemoteApplicative cmd proc) where   -- may need m to be restricted to Monad here
  pure a = Pure a
  (<*>) = Ap

instance Alternative (RemoteApplicative cmd proc) where
   empty       = Empty
   Empty <|> p = p
   m1 <|> m2   = Alt m1 m2
   
data RemoteMonadException = RemoteEmptyException
   deriving (Show, Typeable)                             
                                                         
instance Exception RemoteMonadException                 
      

value :: f a -> Wrapper f a
value  = Value 
                                                  
data Wrapper f a where
    Value :: f a -> Wrapper f a
    Throw' :: f () -> Wrapper f a

instance Applicative f => Functor (Wrapper f) where
    fmap f g = (pure f)<*> g
    
instance Applicative f => Applicative (Wrapper f) where
    pure a = Value $ pure a
    (Value f) <*> (Value g) = Value (f <*> g)
    (Throw' f) <*> g = Throw' f 
    (Value f)  <*> (Throw' g) = Throw' (f *> g)
    
instance Applicative f => Alternative (Wrapper f) where
     empty = Throw' (pure ()) 
     (Throw' g) <|> (Value h) = Value (g *> h)
     (Throw' g) <|> (Throw' h) = Throw' (g *> h)
     (Value g)  <|> _ = Value g
