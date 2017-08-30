# Haskino

Haskino (Haskell Arduino) is our framework for exploring EDSL concepts with the Arduino.

The current version, 0.6, is a new version using a GHC plugin which allows
a Shallow DSL program to be automatically rewritten to a Deep DSL.
This is necessitated changing some of Haskino's Deep API's, and renaming
others.  The legacy version of Haskino is still availalbe at the v0.5 tag.

Haskino examples are now in a seperate repository at [haskino-examples](https://github.com/ku-fpg/haskino-examples).  These are currently examples to be used with the shallow to deep translation system.  Legacy native deep examples can be found in the legacy directory in this repo, but may need some adjustment to work in the 0.6 version.  They will eventually be moved to the haskino-examples repo also.

