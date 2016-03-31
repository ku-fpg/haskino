{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Control.Remote.Monad.Packet.Applicative
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Monad.Packet.Applicative
  ( -- * The remote applicative
    ApplicativePacket(..)
    -- * Utility
  , superCommand
  ) where


import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import qualified Control.Remote.Monad.Packet.Strong as Strong
import Control.Natural

-- | A Remote Applicative, that can encode both commands and procedures, bundled together.

data ApplicativePacket (c :: *) (p :: * -> *) (a :: *) where
   Command   :: c                           -> ApplicativePacket c p ()
   Procedure :: p a                         -> ApplicativePacket c p a
   Zip       :: (x -> y -> z)
             -> ApplicativePacket c p x 
             -> ApplicativePacket c p y     -> ApplicativePacket c p z
   Pure      :: a                           -> ApplicativePacket c p a  

instance Functor (ApplicativePacket c p) where
  fmap f g = pure f <*> g

instance Applicative (ApplicativePacket c p) where
  pure a = Pure a
  g <*> h = Zip ($) g h

-- | This simulates a 'ApplicativePacket', to see if it only contains commands, and if so,
-- returns the static result. The commands still need executed. The term super-command
-- is a play on Hughes' super-combinator terminology.

superCommand :: ApplicativePacket c p a -> Maybe a
superCommand (Pure a)        = pure a
superCommand (Command c)     = pure ()
superCommand (Procedure _)   = Nothing
superCommand (Zip ($) g h)   = ($) <$> superCommand g <*> superCommand h


