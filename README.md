# kansas-amber

kansas-amber is our framework for exploring EDSL concepts with the Arduino.

# Version 0.1 

Version 0.1 (v0.1 in the repo) of Kansas Amber extends hArduino with our
strong Remote Monad concepts, and adds functionality that was not present
in hArduino, such as I2C and Stepper motor control.  It requires Configurable
Firmata instead of the standard Firmata, and beyond that, it requires a fork
of Configurable Firmata which may be found at 
https://github.com/markgrebe/ConfigurableFirmata.

# Version 0.2

Version 0.2 replaces Configurable Firmata with our own Arduino firmware
(Amber Firmware) which does not use the Firmata protocol, but instead uses
a much simpler framed protocol that will be easily extended for our
upcoming research.
