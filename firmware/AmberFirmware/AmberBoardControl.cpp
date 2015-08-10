#include <Arduino.h>
#include "AmberBoardControl.h"
#include "AmberComm.h"
#include "AmberCommands.h"
#include "AmberFirmware.h"
#include "SoftReset.h"

static int handleSetPinMode(int size, byte *msg);
static int handleDelayMillis(int size, byte *msg);
static int handleDelayMicros(int size, byte *msg);
static int handleSystemReset(int size, byte *msg);

int parseBoardControlMessage(int size, byte *msg)
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
    }

static int handleSetPinMode(int size, byte *msg)
    {
    pinMode(msg[1], msg[2]);
    return 3;   
    }

static int handleDelayMillis(int size, byte *msg)
    {
    unsigned long millis;
    memcpy(&millis, &msg[1], 4);
    delay(millis);
    return 5;
    }

static int handleDelayMicros(int size, byte *msg)
    {
    unsigned long millis;
    memcpy(&millis, &msg[1], 4);
    delay(millis);
    return 5;
    }

static int handleSystemReset(int size, byte *msg)
    {
    soft_restart();
    return 1;
    }
