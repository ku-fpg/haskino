{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module:      Control.Remote.Monad.Packet.Alternative
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Packet.Alternative
  ( -- * The remote applicative
    AlternativePacket(..)
  ) where


import           Control.Applicative

-- | A Remote Applicative, that can encode both commands and procedures, bundled together.

data AlternativePacket (cp :: * -> *) (a :: *) where
   Primitive :: cp     a                 -> AlternativePacket cp a
   Zip       :: (x -> y -> z)
             -> AlternativePacket cp x
             -> AlternativePacket cp y   -> AlternativePacket cp z
   Pure      :: a                        -> AlternativePacket cp a
   Alt       :: AlternativePacket cp a
             -> AlternativePacket cp a   -> AlternativePacket cp a
   Empty     ::                             AlternativePacket cp a

instance Functor (AlternativePacket cp) where
  fmap f g = pure f <*> g

instance Applicative (AlternativePacket cp) where
  pure a = Pure a
  g <*> h = Zip ($) g h

instance Alternative (AlternativePacket cp) where
  g <|> h = g `Alt` h
  empty   = Empty
