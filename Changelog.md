# Version 0.6

Version 0.6 is a major upgrade to the Haskino framework.  It introduces a 
GHC compiler plugin which allows Haskino programs to be written in the
Shallow DSL style and automatically be converted to the Deep DSL for
compilation.

Details of the this are presented in our paper:

M. Grebe, D. Young,  and A. Gill, “Rewriting a shallow DSL using a GHC compiler extension,”
in *Proceedings of the 2017 International Conference on Generative Programming: Concepts and Experiences*, inpress

# Version 0.5

This version changes the while function in the remote reference typeclass has been changed to include a intialization of the loop remote reference.  It's type has changed from:

    while                 :: RemoteRef a -> (Expr a -> Expr Bool) -> 
                             (Expr a -> Expr a) -> Arduino () -> Arduino ()

to

    while                 :: RemoteRef a -> Expr a -> (Expr a -> Expr Bool) -> 
                             (Expr a -> Expr a) -> Arduino () -> Arduino ()

Also, a pretty printer for the Arduino monad has been added for debugging.

Details of the this and version 0.4 are vare presented in our paper:

M. Grebe and A. Gill, “Threading the Arduino with Haskell,”
in *Post-Proceedings of Trends in Functional Programming*, inpress

# Version 0.4

Version 0.4 is a major upgrade to the Haskino framework.  It includes a micro kernel in the interpreter which enables multitasking, as well as a trans-compiler which may be used to generate C code for flashing to the Arduino.  This version corresponds to the talk given at TFP 2016.

# Version 0.3

Version 0.3 is a major upgrade to the Haskino framework.  It includes both the
tethered Strong Remote Monad version from Version 0.2, as well as a Deep
EDSL version which allows for standalone operation.

Details of the two versions are presented in our paper:

M. Grebe and A. Gill, “Haskino: A remote monad for programming the Arduino,”
in *Practical Aspects of Declarative Languages*, ser. Lecture Notes in
Computer Science, 2016.

Sample programs are provided in the /System/Hardware/Haskino/SamplePrograms/
directory.  This directory contains three subdirectories.  The Strong examples
subdirectory uses the tethered Strong Remote Monad of Version 2.0.  The Deep
examples subdirectory uses the Deep EDSL.  The Test examples subdirectory
includes very simple tests that were used for unit testing.

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

