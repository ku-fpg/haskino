#include <Arduino.h>
#include <EEPROM.h>

/*
 
  https://github.com/haskino/firmware/EEPROMErase.ino

  Copyright (C) University of Kansas. All rights reserved.

*/

/*==============================================================================
 * SETUP()
 *============================================================================*/
void setup()
{
    // Clear any stored task in EEPROM
    if (EEPROM[ 0 ] == 'H')
        {
        EEPROM[ 0 ] = 0;
        EEPROM[ 1 ] = 0;
        EEPROM[ 2 ] = 0;
        EEPROM[ 3 ] = 0;
        }
}

/*==============================================================================
 * LOOP()
 *============================================================================*/
void loop()
{
}
