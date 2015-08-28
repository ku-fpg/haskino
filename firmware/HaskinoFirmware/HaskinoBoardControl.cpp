#include <Arduino.h>
#include "HaskinoBoardControl.h"
#include "HaskinoComm.h"
#include "HaskinoCommands.h"
#include "HaskinoFirmware.h"
#include "HaskinoScheduler.h"
#include "SoftReset.h"

static bool handleSetPinMode(int size, byte *msg);
static bool handleDelayMillis(int size, byte *msg);
static bool handleDelayMicros(int size, byte *msg);
static bool handleSystemReset(int size, byte *msg);

bool parseBoardControlMessage(int size, byte *msg, byte *local)
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

    if (isRunningTask())
        {
        delayRunningTask(millis);
        }
    else 
        {
        delay(millis);
        }
    return true;
    }

static bool handleDelayMicros(int size, byte *msg)
    {
    unsigned int micros;
    memcpy(&micros, &msg[1], 2);
    delayMicroseconds(micros);
    return false;
    }

static bool handleSystemReset(int size, byte *msg)
    {
    soft_restart();
    return false;
    }
