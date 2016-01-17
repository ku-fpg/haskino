# Haskino

Haskino (Haskell Arduino) is our framework for exploring EDSL concepts with the Arduino.

# Version 0.3

Version 0.3 is a major upgrade to the Haskino framework.  It includes both the
tethered Strong Remote Monad version from Version 0.2, as well as a Deep
EDSL version which allows for standalone operation.

Details of the two versions are presented in our paper:

M. Grebe and A. Gill, “Haskino: A remote monad for programming the Arduino,”
in *Practical Aspects of Declarative Languages*, ser. Lecture Notes in
Computer Science, 2016.

Sample programs are provided in the /System/Hardware/Haskino/SamplePrograms/
directory.  This directory contains three subdirectories.  The Sha

# Version 0.2

Version 0.2 replaces Configurable Firmata with our own Arduino firmware
(Haskino Firmware) which does not use the Firmata protocol, but instead uses
a much simpler framed protocol that will be easily extended for our
upcoming research.

# Version 0.1 

Version 0.1 (v0.1 in the repo) of Haskino extends hArduino with our
strong Remote Monad concepts, and adds functionality that was not present
in hArduino, such as I2C and Stepper motor control.  It requires Configurable
Firmata instead of the standard Firmata, and beyond that, it requires a fork
of Configurable Firmata which may be found at 
https://github.com/markgrebe/ConfigurableFirmata.

