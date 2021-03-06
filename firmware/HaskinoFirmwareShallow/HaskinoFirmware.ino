#include <Arduino.h>
#include <Wire.h>
#include <math.h>
#include "HaskinoComm.h"
#include "HaskinoCommands.h"
#include "HaskinoBoardStatus.h"

/*
 
  https://github.com/haskino/firmware/HaskinoFirmware.ino

  Copyright (C) University of Kansas. All rights reserved.

*/

/*==============================================================================
 * SETUP()
 *============================================================================*/
void setup()
{
    Serial.begin(115200);
}

/*==============================================================================
 * LOOP()
 *============================================================================*/
void loop()
{
    while (Serial.available()) 
        {
        handleInput();
        }
}
