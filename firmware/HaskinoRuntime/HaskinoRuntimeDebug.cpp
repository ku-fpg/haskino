/******************************************************************************
 *
 * Module      :  HaskinoRuntimeDebug
 * Copyright   :  (c) University of Kansas
 * License     :  BSD3
 * Stability   :  experimental
 *
 * Haskino Runtime Debug module
 *****************************************************************************/

#include <Arduino.h>
#include "HaskinoRuntime.h"
#include "HaskinoRuntimeList.h"

// Debug routines

void debug(uint8_t *s)
    {
    bool opened = false;

    if (!opened)
        {
        Serial.begin(115200);
        opened = true;
        }

    for (int i=0; i<s[1]; i++)
        Serial.write(s[2+i]);
    Serial.write('\n');
    listFree(s);
    }
    
