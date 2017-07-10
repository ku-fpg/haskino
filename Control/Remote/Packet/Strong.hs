{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeOperators  #-}

{-|
Module:      Control.Remote.Monad.Packet.Strong
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Packet.Strong where

import           Control.Natural


-- | A Strong Packet, that can encode a list of commands, terminated by an optional procedure.

data StrongPacket (prim :: * -> *) (a :: *) where
   Command   :: prim () -> StrongPacket prim () -> StrongPacket prim ()
   Procedure :: prim a -> StrongPacket prim a
   Done      ::           StrongPacket prim ()

-- | A Hughes-style version of 'StrongPacket', with efficent append.
newtype HStrongPacket (prim :: * -> *) = HStrongPacket (StrongPacket prim ~> StrongPacket prim)
