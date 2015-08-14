#include <Wire.h>

#include <Arduino.h>
#include "AmberComm.h"
#include "AmberCommands.h"
#include "AmberBoardStatus.h"

/*
 
  https://github.com/kansas-amber/firmware/AmberFirmware.ino

  Copyright (C) University of Kansas. All rights reserved.

*/

/*==============================================================================
 * SETUP()
 *============================================================================*/
void setup()
{
    Serial.begin(115200);
    sendVersionReply();
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
#if 0
            schedulerRunTasks();
#endif    
            }
        }
    if (!processingMessage()) 
        {
#if 0
        schedulerRunTasks();
#endif
        }
}
