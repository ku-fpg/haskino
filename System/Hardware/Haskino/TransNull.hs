-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.Protocol
--                Based on System.Hardware.Arduino.Protocol
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Internal representation of the Haskino Firmware protocol.
-------------------------------------------------------------------------------
{-# LANGUAGE GADTs #-}

module System.Hardware.Haskino.TransNull where

import Data.Bits (xor,shiftR)
import Data.Int (Int8, Int16, Int32)
import Data.List
import Data.Word (Word8, Word16, Word32)

import Control.Monad.State

import Control.Remote.Monad
import Control.Remote.Monad.Types as T

import System.Hardware.Haskino.Data
import System.Hardware.Haskino.Expr
import System.Hardware.Haskino.Utils

transCommand :: ArduinoCommand -> ArduinoCommand
transCommand SystemReset = SystemReset
transCommand (SetPinModeE p m) = SetPinModeE (transExpr p) (transExpr m)
transCommand (DigitalWriteE p b) = DigitalWriteE (transExpr p) (transExpr b)
transCommand (DigitalPortWriteE p b m) = DigitalPortWriteE (transExpr p) (transExpr b) (transExpr m)
transCommand (AnalogWriteE p w) = AnalogWriteE (transExpr p) (transExpr w)
transCommand (ToneE p f (Just d)) = ToneE (transExpr p) (transExpr f) (Just (transExpr d))
transCommand (ToneE p f Nothing) = ToneE  (transExpr p) (transExpr f) Nothing
transCommand (NoToneE p) = NoToneE (transExpr p)
transCommand (I2CWrite sa w8s) = I2CWrite (transExpr sa) (transExpr w8s)
transCommand I2CConfig = I2CConfig
transCommand (StepperSetSpeedE st sp) = StepperSetSpeedE (transExpr st) (transExpr sp)
transCommand (ServoDetachE sv) = ServoDetachE (transExpr sv)
transCommand (ServoWriteE sv w) = ServoWriteE (transExpr sv) (transExpr w)
transCommand (ServoWriteMicrosE sv w) = ServoWriteMicrosE (transExpr sv) (transExpr w)
transCommand (DeleteTaskE tid) = DeleteTaskE (transExpr tid)
transCommand (ScheduleTaskE tid tt) = ScheduleTaskE (transExpr tid) (transExpr tt)
transCommand ScheduleReset = ScheduleReset
transCommand (AttachIntE p t m) = AttachIntE (transExpr p) (transExpr t) (transExpr m)
transCommand (DetachIntE p) = DetachIntE (transExpr p)
transCommand (Interrupts) = Interrupts
transCommand (NoInterrupts) = NoInterrupts
transCommand (GiveSemE id) = GiveSemE (transExpr id)
transCommand (TakeSemE id) = TakeSemE (transExpr id)
transCommand (CreateTaskE tid m) = CreateTaskE (transExpr tid) m
transCommand (WriteRemoteRefB (RemoteRefB i) e) = WriteRemoteRefB (RemoteRefB i) (transExpr e)
transCommand (WriteRemoteRefW8 (RemoteRefW8 i) e) = WriteRemoteRefW8 (RemoteRefW8 i) (transExpr e)
transCommand (WriteRemoteRefW16 (RemoteRefW16 i) e) = WriteRemoteRefW16 (RemoteRefW16 i) (transExpr e)
transCommand (WriteRemoteRefW32 (RemoteRefW32 i) e) = WriteRemoteRefW32 (RemoteRefW32 i) (transExpr e)
transCommand (WriteRemoteRefI8 (RemoteRefI8 i) e) = WriteRemoteRefI8 (RemoteRefI8 i) (transExpr e)
transCommand (WriteRemoteRefI16 (RemoteRefI16 i) e) = WriteRemoteRefI16 (RemoteRefI16 i) (transExpr e)
transCommand (WriteRemoteRefI32 (RemoteRefI32 i) e) = WriteRemoteRefI32 (RemoteRefI32 i) (transExpr e)
transCommand (WriteRemoteRefL8 (RemoteRefL8 i) e) = WriteRemoteRefL8 (RemoteRefL8 i) (transExpr e)
transCommand (WriteRemoteRefFloat (RemoteRefFloat i) e) = WriteRemoteRefFloat (RemoteRefFloat i) (transExpr e)
transCommand (ModifyRemoteRefB (RemoteRefB i) f) = ModifyRemoteRefB (RemoteRefB i) (transExpr f)
transCommand (ModifyRemoteRefW8 (RemoteRefW8 i) f) = ModifyRemoteRefW8 (RemoteRefW8 i) (transExpr f)
transCommand (ModifyRemoteRefW16 (RemoteRefW16 i) f) = ModifyRemoteRefW16 (RemoteRefW16 i) (transExpr f)
transCommand (ModifyRemoteRefW32 (RemoteRefW32 i) f) = ModifyRemoteRefW32 (RemoteRefW32 i) (transExpr f)
transCommand (ModifyRemoteRefI8 (RemoteRefI8 i) f) = ModifyRemoteRefI8 (RemoteRefI8 i) (transExpr f)
transCommand (ModifyRemoteRefI16 (RemoteRefI16 i) f) = ModifyRemoteRefI16 (RemoteRefI16 i) (transExpr f)
transCommand (ModifyRemoteRefI32 (RemoteRefI32 i) f) = ModifyRemoteRefI32 (RemoteRefI32 i) (transExpr f)
transCommand (ModifyRemoteRefL8 (RemoteRefL8 i) f) = ModifyRemoteRefL8 (RemoteRefL8 i) (transExpr f)
transCommand (ModifyRemoteRefFloat (RemoteRefFloat i) f) = ModifyRemoteRefFloat (RemoteRefFloat i) (transExpr f)
transCommand (WhileRemoteRefB (RemoteRefB i) iv bf uf cb) =
    WhileRemoteRefB (RemoteRefB i) (transExpr iv) (transExpr bf) (transExpr uf) (transNull cb)
transCommand (WhileRemoteRefW8 (RemoteRefW8 i) iv bf uf cb) =
    WhileRemoteRefW8 (RemoteRefW8 i) (transExpr iv) (transExpr bf) (transExpr uf)  (transNull cb)
transCommand (WhileRemoteRefW16 (RemoteRefW16 i) iv bf uf cb) =
    WhileRemoteRefW16 (RemoteRefW16 i) (transExpr iv) (transExpr bf) (transExpr uf)  (transNull cb)
transCommand (WhileRemoteRefW32 (RemoteRefW32 i) iv bf uf cb) =
    WhileRemoteRefW32 (RemoteRefW32 i) (transExpr iv) (transExpr bf) (transExpr uf)  (transNull cb)
transCommand (WhileRemoteRefI8 (RemoteRefI8 i) iv bf uf cb) =
    WhileRemoteRefI8 (RemoteRefI8 i) (transExpr iv) (transExpr bf) (transExpr uf)  (transNull cb)
transCommand (WhileRemoteRefI16 (RemoteRefI16 i) iv bf uf cb) =
    WhileRemoteRefI16 (RemoteRefI16 i) (transExpr iv) (transExpr bf) (transExpr uf)  (transNull cb)
transCommand (WhileRemoteRefI32 (RemoteRefI32 i) iv bf uf cb) =
    WhileRemoteRefI32 (RemoteRefI32 i) (transExpr iv) (transExpr bf) (transExpr uf)  (transNull cb)
transCommand (WhileRemoteRefFloat (RemoteRefFloat i) iv bf uf cb) =
    WhileRemoteRefFloat (RemoteRefFloat i) (transExpr iv) (transExpr bf) (transExpr uf)  (transNull cb)
transCommand (WhileRemoteRefL8 (RemoteRefL8 i) iv bf uf cb) = 
    WhileRemoteRefL8 (RemoteRefL8 i) (transExpr iv) (transExpr bf) (transExpr uf)  (transNull cb)
transCommand (Loop cb) = Loop (transNull cb)
transCommand (LoopE cb) = Loop (transNull cb)
-- To Do - Go inside forin command
transCommand (ForInE ws f) = ForInE ws f
transCommand (IfThenElse e cb1 cb2) = IfThenElse (transExpr e) (transNull cb1) (transNull cb2)

transProcedure :: ArduinoProcedure a -> ArduinoProcedure a
transProcedure p = p

transExpr :: Expr a -> Expr a
transExpr e = e

transNull :: Arduino a -> Arduino a
transNull (Arduino commands) = Arduino $ transMonad commands
  where
      transAppl :: RemoteApplicative ArduinoCommand ArduinoProcedure a -> RemoteApplicative ArduinoCommand ArduinoProcedure a
      transAppl (T.Command cmd) = T.Command $ transCommand cmd
      transAppl (T.Procedure p) = T.Procedure $ transProcedure p
      transAppl (T.Ap a1 a2) = T.Ap (transAppl a1) (transAppl a2)
      transAppl (T.Pure a) = T.Pure a

      transMonad :: RemoteMonad ArduinoCommand ArduinoProcedure a -> RemoteMonad ArduinoCommand ArduinoProcedure a
      transMonad (T.Appl app) = T.Appl (transAppl app)
      transMonad (T.Bind m k) = T.Bind (transMonad m) k
      transMonad (T.Ap' m1 m2) = T.Ap' (transMonad m1) (transMonad m2)
