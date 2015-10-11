#include <Arduino.h>
#include "HaskinoBoardControl.h"
#include "HaskinoCodeBlock.h"
#include "HaskinoComm.h"
#include "HaskinoCommands.h"
#include "HaskinoConfig.h"
#include "HaskinoExpr.h"
#include "HaskinoFirmware.h"
#include "HaskinoScheduler.h"
#include "SoftReset.h"

static bool handleSystemReset(int size, const byte *msg, byte *local);
static bool handleSetPinMode(int size, const byte *msg, byte *local);
static bool handleDelayMillis(int size, const byte *msg, byte *local);
static bool handleDelayMicros(int size, const byte *msg, byte *local);
static bool handleWhile(int size, const byte *msg, byte *local);
static bool handleIfThenElse(int size, const byte *msg, byte *local);

bool parseBoardControlMessage(int size, const byte *msg, byte *local)
    {
    switch (msg[0] ) 
        {
        case BC_CMD_SYSTEM_RESET:
            return handleSystemReset(size, msg, local);
            break;
        case BC_CMD_SET_PIN_MODE:
            return handleSetPinMode(size, msg, local);
            break;
        case BC_CMD_DELAY_MILLIS:
            return handleDelayMillis(size, msg, local);
            break;
        case BC_CMD_DELAY_MICROS:
            return handleDelayMicros(size, msg, local);
            break;
        case BC_CMD_WHILE:
            return handleWhile(size, msg, local);
            break;
        case BC_CMD_IF_THEN_ELSE:
            return handleIfThenElse(size, msg, local);
            break;
        }
    return false;
    }

static bool handleSetPinMode(int size, const byte *msg, byte *local)
    {
    byte *expr = (byte *) &msg[1];
    byte pinNo = evalWord8Expr(&expr, local);
    pinMode(pinNo, *expr);
    return false;
    }

static bool handleDelayMillis(int size, const byte *msg, byte *local)
    {
    byte *expr = (byte *) &msg[2];
    uint32_t millis = evalWord32Expr(&expr, local);

    if (isRunningTask() && !isCodeBlock())
        {
        delayRunningTask(millis);
        }
    else 
        {
        delay(millis);
        if (!isRunningTask() && !isCodeBlock())
            {
            sendReply(0, BC_RESP_DELAY, NULL, NULL,0);
            }
        }
    return true;
    }

static bool handleDelayMicros(int size, const byte *msg, byte *local)
    {
    byte *expr = (byte *) &msg[2];
    uint32_t micros = evalWord16Expr(&expr, local);
    delayMicroseconds(micros);
    if (!isRunningTask() && !isCodeBlock())
        {
        sendReply(0, BC_RESP_DELAY, NULL, NULL,0);
        }
    return false;
    }

static bool handleSystemReset(int size, const byte *msg, byte *local)
    {
    soft_restart();
    return false;
    }

static bool handleWhile(int size, const byte *msg, byte *local)
    {
    byte *expr = (byte *) &msg[1];
    bool condition = evalBoolExpr(&expr, local);
    byte *codeBlock = expr;
    int whileSize = size - (expr - msg);

    while (condition)
        {
        runCodeBlock(whileSize, codeBlock, local);

        expr = (byte *) &msg[1];
        condition = evalBoolExpr(&expr, local);
        }

    return false;
    }

static bool handleIfThenElse(int size, const byte *msg, byte *local)
    {
    uint16_t thenSize, elseSize;
    memcpy(&thenSize, &msg[1], sizeof(thenSize));
    byte *expr = (byte *) &msg[3];
    bool condition = evalBoolExpr(&expr, local);
    byte *codeBlock = expr;

    if (condition)
        {
        runCodeBlock(thenSize, codeBlock, local);
        }
    else
        {
        elseSize = size - (thenSize + (codeBlock - msg));
        runCodeBlock(elseSize, codeBlock + thenSize, local);
        }

    return false;
    }

