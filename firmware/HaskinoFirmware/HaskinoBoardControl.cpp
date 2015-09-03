#include <Arduino.h>
#include "HaskinoBoardControl.h"
#include "HaskinoComm.h"
#include "HaskinoCommands.h"
#include "HaskinoExpr.h"
#include "HaskinoFirmware.h"
#include "HaskinoScheduler.h"
#include "SoftReset.h"

static bool handleSetPinMode(int size, const byte *msg);
static bool handleDelayMillis(int size, const byte *msg);
static bool handleDelayMicros(int size, const byte *msg);
static bool handleSystemReset(int size, const byte *msg);
static bool handleSetPinModeE(int size, const byte *msg);
static bool handleDelayMillisE(int size, const byte *msg);
static bool handleDelayMicrosE(int size, const byte *msg);

bool parseBoardControlMessage(int size, const byte *msg, byte *local)
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
        case BC_CMD_SET_PIN_MODE_E:
            return handleSetPinModeE(size, msg);
            break;
        case BC_CMD_DELAY_MILLIS_E:
            return handleDelayMillisE(size, msg);
            break;
        case BC_CMD_DELAY_MICROS_E:
            return handleDelayMicrosE(size, msg);
            break;
        }
    return false;
    }

static bool handleSetPinMode(int size, const byte *msg)
    {
    pinMode(msg[1], msg[2]);
    return false;
    }

static bool handleDelayMillis(int size, const byte *msg)
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

static bool handleDelayMicros(int size, const byte *msg)
    {
    unsigned int micros;
    memcpy(&micros, &msg[1], 2);
    delayMicroseconds(micros);
    return false;
    }

static bool handleSetPinModeE(int size, const byte *msg)
    {
    byte *expr = (byte *) &msg[1];
    byte pinNo = evalWord8Expr(&expr);
    pinMode(pinNo, msg[2]);
    return false;
    }

static bool handleDelayMillisE(int size, const byte *msg)
    {
    byte *expr = (byte *) &msg[1];
    uint32_t millis = evalWord32Expr(&expr);

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

static bool handleDelayMicrosE(int size, const byte *msg)
    {
    byte *expr = (byte *) &msg[1];
    uint32_t micros = evalWord16Expr(&expr);
    delayMicroseconds(micros);
    return false;
    }

static bool handleSystemReset(int size, const byte *msg)
    {
    soft_restart();
    return false;
    }
