/******************************************************************************
 *
 * Module      :  HaskinoRuntimeI2C
 * Copyright   :  (c) University of Kansas
 * License     :  BSD3
 * Stability   :  experimental
 *
 * Haskino Runtime Serial module
 *****************************************************************************/

#include <Arduino.h>
#include <Wire.h>
#include "HaskinoRuntime.h"
#include "HaskinoRuntimeList.h"

// Serial routines

static HardwareSerial *getDev(uint8_t p)
    {
    switch(p)
        {
#ifdef HAVE_HWSERIAL0
        case 0:
            return &Serial;
#endif
#ifdef HAVE_HWSERIAL1
        case 1:
            return &Serial1;
#endif
#ifdef HAVE_HWSERIAL2
        case 2:
            return &Serial2;
#endif
#ifdef HAVE_HWSERIAL3
        case 3:
            return &Serial3;
#endif
        default:
            return NULL;
        }
    }

uint8_t serialAvailable(uint8_t p)
    {
    HardwareSerial *dev = getDev(p);

    if (!dev)
        return 0;

    return dev->available();
    }

void serialBegin(uint8_t p, uint32_t r)
    {
    HardwareSerial *dev = getDev(p);

    if (!dev)
        return;

    dev->begin(r);
    }

void serialEnd(uint8_t p)
    {
    HardwareSerial *dev = getDev(p);

    if (!dev)
        return;

    dev->end();
    }

void serialWrite(uint8_t p, uint8_t w)
    {
    HardwareSerial *dev = getDev(p);

    if (!dev)
        return;

    dev->write(w);
    }

void serialWriteList(uint8_t p, uint8_t *w8s)
    {
    HardwareSerial *dev = getDev(p);

    if (!dev)
        return;

    if (w8s[1] != 0)
        {
        dev->write(&w8s[2], w8s[1]);
        }
    listFree(w8s);
    }

uint32_t serialRead(uint8_t p)
    {
    HardwareSerial *dev = getDev(p);

    if (!dev)
        return -1;

    return(dev->read());
    }

uint8_t *serialReadList(uint8_t p)
    {
    HardwareSerial *dev = getDev(p);

    if (!dev)
        return emptyList;

    byte *localMem, *local;
    int byteAvail;

    byteAvail = dev->available();

    localMem = listAlloc(byteAvail);
    if (localMem == NULL)
        return emptyList;

    local = &localMem[2];

    localMem[1] = byteAvail;

    for (int i = 0; i < byteAvail; i++)
        {
        *local++ = dev->read();
        }

    return localMem;
    }

