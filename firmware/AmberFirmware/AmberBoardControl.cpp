#include <Arduino.h>
#include "AmberBoardControl.h"
#include "AmberComm.h"
#include "AmberCommands.h"
#include "AmberFirmware.h"
#include "SoftReset.h"

static void handleSetPinMode(int size, byte *msg);
static void handleDelayMillis(int size, byte *msg);
static void handleDelayMicros(int size, byte *msg);
static void handleSystemReset(int size, byte *msg);

void parseBoardControlMessage(int size, byte *msg)
    {
    switch (msg[0] ) 
        {
        case BC_CMD_SET_PIN_MODE:
            handleSetPinMode(size, msg);
            break;
        case BC_CMD_DELAY_MILLIS:
            handleDelayMillis(size, msg);
            break;
        case BC_CMD_DELAY_MICROS:
            handleDelayMicros(size, msg);
            break;
        case BC_CMD_SYSTEM_RESET:
            handleSystemReset(size, msg);
            break;
        }
    }

static void handleSetPinMode(int size, byte *msg)
    {
    pinMode(msg[1], msg[2]);   
    }

static void handleDelayMillis(int size, byte *msg)
    {
    unsigned long millis;
    memcpy(&millis, &msg[1], 4);
    delay(millis);
    }

static void handleDelayMicros(int size, byte *msg)
    {
    unsigned long millis;
    memcpy(&millis, &msg[1], 4);
    delay(millis);
    }

static void handleSystemReset(int size, byte *msg)
    {
    soft_restart();
    }
