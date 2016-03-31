{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

{-|
Module:      Control.Remote.Monad.Packet.Weak
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Monad.Packet.Weak where

import Control.Remote.Monad.Packet.Transport 

-- | A Weak Packet, that can encode a command or a procedure.

data WeakPacket (c :: *) (p :: * -> *) (a :: *) where
   Command   :: c   -> WeakPacket c p ()
   Procedure :: p a -> WeakPacket c p a

deriving instance (Show c, Show (p a)) => Show (WeakPacket c p a)

instance (Read c, Read (Transport p)) => Read (Transport (WeakPacket c p)) where
  readsPrec d = readParen (d > 10) $ \ r0 ->
        [ (Transport $ Command c,r2)
        | ("Command",r1) <- lex r0
        , (c,r2)         <- readsPrec 11 r1
        ] ++
        [ (Transport $ Procedure p,r2)
        | ("Procedure",r1) <- lex r0
        , (Transport p,r2) <- readsPrec 11 r1
        ] 
