#include <Wire.h>

#include <Arduino.h>
#include "HaskinoComm.h"
#include "HaskinoCommands.h"
#include "HaskinoBoardStatus.h"
#include "HaskinoScheduler.h"

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
    sendVersionReply(NULL);
}

/*==============================================================================
 * LOOP()
 *============================================================================*/
void loop()
{
    while (Serial.available()) 
        {
        handleInput();
        if (!processingMessage()) 
            {
            schedulerRunTasks();
            }
        }
    if (!processingMessage()) 
        {
        schedulerRunTasks();
        }
}
