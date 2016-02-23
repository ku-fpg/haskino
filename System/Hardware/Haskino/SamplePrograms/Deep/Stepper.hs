module System.Hardware.Haskino.SamplePrograms.Deep.Stepper where

import System.Hardware.Haskino
import System.Hardware.Haskino.Data

import Control.Monad (forever)


runStep :: IO ()
runStep = withArduino True "/dev/cu.usbmodem1421" $ do
  let numSteps  = lit 200 
  let stepSpeed = 50
  stepper <- stepper4PinE numSteps 22 23 24 25
  stepperSetSpeedE stepper stepSpeed
  forever $ stepperStepE stepper (lit 200)  
