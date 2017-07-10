{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

{-|
Module:      Control.Remote.Util
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Util
  ( Wrapper(..)
  , RemoteMonadException(..)
  , KnownResult(..)
  , value
  ) where


import           Control.Applicative
import           Control.Monad.Catch
import           Data.Typeable


data Wrapper f a where
    Value :: f a -> Wrapper f a
    Throw' :: f () -> Wrapper f a

instance Applicative f => Functor (Wrapper f) where
    fmap f g = pure f <*> g

instance Applicative f => Applicative (Wrapper f) where
    pure a = Value $ pure a
    (Value f) <*> (Value g) = Value (f <*> g)
    (Throw' f) <*> _ = Throw' f
    (Value f)  <*> (Throw' g) = Throw' (f *> g)

instance Applicative f => Alternative (Wrapper f) where
     empty                     = Throw' (pure ())
     (Throw' g) <|> (Value h)  = Value (g *> h)
     (Throw' g) <|> (Throw' h) = Throw' (g *> h)
     (Value g)  <|> _          = Value g

value :: f a -> Wrapper f a
value  = Value

data RemoteMonadException = RemoteEmptyException
   deriving (Show, Typeable)

instance Exception RemoteMonadException

-- | Can we dynamically extract the 'result' of a functor, without evaluation.

class KnownResult f where
  knownResult :: f a -> Maybe a
  knownResult _ = Nothing
