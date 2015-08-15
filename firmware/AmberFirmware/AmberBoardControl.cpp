#include <Arduino.h>
#include "AmberBoardControl.h"
#include "AmberComm.h"
#include "AmberCommands.h"
#include "AmberFirmware.h"
#include "SoftReset.h"

static bool handleSetPinMode(int size, byte *msg);
static bool handleDelayMillis(int size, byte *msg);
static bool handleDelayMicros(int size, byte *msg);
static bool handleSystemReset(int size, byte *msg);

bool parseBoardControlMessage(int size, byte *msg)
    {
    switch (msg[0] ) 
        {
        case BC_CMD_SET_PIN_MODE:
            return handleSetPinMode(size, msg);
            break;
        case BC_CMD_DELAY_MILLIS:
            return handleDelayMillis(size, msg);
            break;
        case BC_CMD_DELAY_MICROS:
            return handleDelayMicros(size, msg);
            break;
        case BC_CMD_SYSTEM_RESET:
            return handleSystemReset(size, msg);
            break;
        }
    return false;
    }

static bool handleSetPinMode(int size, byte *msg)
    {
    pinMode(msg[1], msg[2]);
    return false;
    }

static bool handleDelayMillis(int size, byte *msg)
    {
    unsigned long millis;
    memcpy(&millis, &msg[1], 4);
    delay(millis);
    return true;
    }

static bool handleDelayMicros(int size, byte *msg)
    {
    unsigned long millis;
    memcpy(&millis, &msg[1], 4);
    delay(millis);
    return false;
    }

static bool handleSystemReset(int size, byte *msg)
    {
    soft_restart();
    return false;
    }
