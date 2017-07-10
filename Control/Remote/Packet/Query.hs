{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module:      Control.Remote.Monad.Packet.Query
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Packet.Query
  ( -- * The remote applicative
    QueryPacket(..)
  ) where

-- | A Remote Applicative, that can encode both commands and procedures, bundled together.

data QueryPacket (cp :: * -> *) (a :: *) where
   Primitive :: cp  a            -> QueryPacket cp a
   Zip       :: (x -> y -> z)
             -> QueryPacket cp x
             -> QueryPacket cp y -> QueryPacket cp z
   Pure      :: a                -> QueryPacket cp a

instance Functor (QueryPacket cp) where
  fmap f g = pure f <*> g

instance Applicative (QueryPacket cp) where
  pure a = Pure a
  g <*> h = Zip ($) g h
