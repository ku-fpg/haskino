-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.DeepArduino.SamplePrograms.Servo
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Demonstrates basic Servo motor control
-------------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables      #-}

module System.Hardware.Arduino.SamplePrograms.Servo where

import Control.Monad       (forever)
import Data.Char           (toLower)

import System.Hardware.DeepArduino
import System.Hardware.DeepArduino.Parts.Servo

-- | Control a servo, by executing user requests of blade movement.  We allow 3 user commands:
--
--    * @l@ to swipe from angle-0 to 180;
--
--    * @r@ to swipe from angle-180 to 0;
--
--    * Or any number between @0@ to @180@, which puts the servo to the desired position.
--
-- Almost any servo motor would work with this example, though you should make sure to adjust min/max pulse durations
-- in the 'attach' command to match the datasheet of the servo you have. In this example, we have used the HS-55 feather
-- servo (<http://www.servocity.com/html/hs-55_sub-micro.html>), which has the values 600 to 2400 micro-seconds.
--
-- To connect the servo to the Arduino, simply connect the VCC (red) and the GND (black) appropriately, and the signal line (white)
-- to any SERVO capable pin, in this example we're using pin number 9:
--
--  <<http://github.com/LeventErkok/hArduino/raw/master/System/Hardware/Arduino/SamplePrograms/Schematics/Servo.png>>
servo :: IO ()
servo = do
    conn <- openArduino True "/dev/cu.usbmodem1421"
    -- Create the Servo structure and get the servo init function
    let (s,init) = attach (digital 9) (Just 600) (Just 2400)
    -- Send the servoinit to the arduino
    send conn init
    forever (demo conn s)
 where 
    demo c s = do 
        putStr "Enter l, r or the desired servo angle: "
        a <- getLine
        let ang = read a :: Int
            move a = do
                setAngle s a
                delay 100
            ms =  case (map toLower a, ang) of
                     ("l", _) -> mapM_ move [0 .. 180]
                     ("r", _) -> mapM_ move [180, 179 .. 0]
                     (_,  v) | 0 <= v && v <= 180 -> setAngle s v
                     (_, _)  -> return ()
        send c ms


-- | Control a servo, as guided by the input read from a potentiometer. The set-up is similar to the 'servo' example
-- above, except instead of querying the user for the angle, we use the readings from a potentiometer connected to
-- analog input number 2. We used a 10 KOhm potentiometer, but other pots would work just as well too:
--
--  <<http://github.com/LeventErkok/hArduino/raw/master/System/Hardware/Arduino/SamplePrograms/Schematics/ServoAnalog.png>>
servoAnalog :: IO ()
servoAnalog = do
    conn <- openArduino False "/dev/cu.usbmodem1421"
    -- Create the Servo structure and get the servo init function
    let (s,init) = attach (digital 9) (Just 600) (Just 2400)
        pot = analog 2
    -- Send the servoinit to the arduino
    send conn init
    -- Setup the pot input
    send conn $ do
      setPinMode pot ANALOG
      analogReport pot True
    putStrLn "Adjust the potentiometer to control the servo!"
    forever $ send conn (demo s pot)
 where 
    demo s pot = do 
          v <- analogRead pot
          setAngle s (fromIntegral $ cvt v)
          delay 100
    -- Analog input will be from 0 to 1023; convert it to
    -- angles, mapping 1023 to 0-degrees, and 0 to 180
    cvt i = ((1023-i) * 180) `div` 1023

