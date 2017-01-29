{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Control.Remote.Monad.Packet.Alternative
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Monad.Packet.Alternative
  ( -- * The remote applicative
    AlternativePacket(..)
    -- * Utility
  , superCommand
  ) where


import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import qualified Control.Remote.Monad.Packet.Strong as Strong
import Control.Natural



-- | A Remote Applicative, that can encode both commands and procedures, bundled together.

data AlternativePacket (c :: *) (p :: * -> *) (a :: *) where
   Command   :: c                           -> AlternativePacket c p ()
   Procedure :: p a                         -> AlternativePacket c p a
   Zip       :: (x -> y -> z)
             -> AlternativePacket c p x 
             -> AlternativePacket c p y     -> AlternativePacket c p z
   Pure      :: a                           -> AlternativePacket c p a  
   Alt       :: AlternativePacket c p a
             -> AlternativePacket c p a     -> AlternativePacket c p a
   Empty     ::                                AlternativePacket c p a

instance Functor (AlternativePacket c p) where
  fmap f g = pure f <*> g

instance Applicative (AlternativePacket c p) where
  pure a = Pure a
  g <*> h = Zip ($) g h

instance Alternative (AlternativePacket c p) where
  g <|> h = g `Alt` h
  empty   = Empty

-- | This simulates a 'AlternativePacket', to see if it only contains commands, and if so,
-- returns the static result. The commands still need executed. The term super-command
-- is a play on Hughes' super-combinator terminology.

superCommand :: AlternativePacket c p a -> Maybe a
superCommand (Pure a)        = pure a
superCommand (Command c)     = pure ()
superCommand (Procedure _)   = Nothing
superCommand (Alt g h)       = Nothing -- TODO for now
superCommand (Zip ($) g h)   = ($) <$> superCommand g <*> superCommand h


