/******************************************************************************
 *
 * Module      :  HaskinoRuntimeI2C
 * Copyright   :  (c) University of Kansas
 * License     :  BSD3
 * Stability   :  experimental
 *
 * Haskino Runtime I2C module
 *****************************************************************************/

#include <Arduino.h>
#include <Wire.h>
#include "HaskinoRuntime.h"
#include "HaskinoRuntimeList.h"

// I2C routines

void i2cWrite(uint8_t sa, uint8_t *w8s)
    {
    if (w8s[1] != 0)
        {
        Wire.beginTransmission(sa);
        Wire.write(&w8s[2], w8s[1]);
        Wire.endTransmission();
        delayMicroseconds(70);
        }
    listFree(w8s);
    }
    
uint8_t *i2cRead(uint8_t sa, uint8_t byteCount)
    {
    byte *localMem, *local;
    int byteAvail;

    Wire.requestFrom((int) sa, (int) byteCount);
    byteAvail = Wire.available();

    localMem = listAlloc(byteAvail);
    if (localMem == NULL)
        return NULL;

    local = &localMem[2];

    localMem[1] = byteAvail;

    for (int i = 0; i < byteAvail; i++)
        { 
        *local++ = Wire.read();
        }

    return localMem;
    }
    
void i2cConfig()
    {
    Wire.begin();
    delay(10);
    }
