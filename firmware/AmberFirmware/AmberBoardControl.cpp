#include <Arduino.h>
#include "AmberBoardControl.h"
#include "AmberComm.h"
#include "AmberCommands.h"
#include "AmberFirmware.h"
#include "SoftReset.h"

static void handleSetPinMode(int size, unsigned char *msg);
static void handleDelayMillis(int size, unsigned char *msg);
static void handleDelayMicros(int size, unsigned char *msg);
static void handleSystemReset(int size, unsigned char *msg);

void parseBoardControlMessage(int size, unsigned char *msg)
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

static void handleSetPinMode(int size, unsigned char *msg)
    {
    pinMode(msg[1], msg[2]);   
    }

static void handleDelayMillis(int size, unsigned char *msg)
    {
    unsigned long millis;
    memcpy(&millis, &msg[1], 4);
    delay(millis);
    }

static void handleDelayMicros(int size, unsigned char *msg)
    {
    unsigned long millis;
    memcpy(&millis, &msg[1], 4);
    delay(millis);
    }

static void handleSystemReset(int size, unsigned char *msg)
    {
    soft_restart();
    }
