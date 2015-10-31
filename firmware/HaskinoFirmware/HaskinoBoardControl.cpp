#include <Arduino.h>
#include "HaskinoBoardControl.h"
#include "HaskinoCodeBlock.h"
#include "HaskinoComm.h"
#include "HaskinoCommands.h"
#include "HaskinoConfig.h"
#include "HaskinoExpr.h"
#include "HaskinoFirmware.h"
#include "HaskinoRefs.h"
#include "HaskinoScheduler.h"
#include "SoftReset.h"

static bool handleSystemReset(int size, const byte *msg, CONTEXT *context);
static bool handleSetPinMode(int size, const byte *msg, CONTEXT *context);
static bool handleDelayMillis(int size, const byte *msg, CONTEXT *context);
static bool handleDelayMicros(int size, const byte *msg, CONTEXT *context);
static bool handleLoop(int size, const byte *msg, CONTEXT *context);
static bool handleWhile(int size, const byte *msg, CONTEXT *context);
static bool handleIfThenElse(int size, const byte *msg, CONTEXT *context);

bool parseBoardControlMessage(int size, const byte *msg, CONTEXT *context)
    {
    switch (msg[0] ) 
        {
        case BC_CMD_SYSTEM_RESET:
            return handleSystemReset(size, msg, context);
            break;
        case BC_CMD_SET_PIN_MODE:
            return handleSetPinMode(size, msg, context);
            break;
        case BC_CMD_DELAY_MILLIS:
            return handleDelayMillis(size, msg, context);
            break;
        case BC_CMD_DELAY_MICROS:
            return handleDelayMicros(size, msg, context);
            break;
        case BC_CMD_LOOP:
            return handleLoop(size, msg, context);
            break;
        case BC_CMD_WHILE:
            return handleWhile(size, msg, context);
            break;
        case BC_CMD_IF_THEN_ELSE:
            return handleIfThenElse(size, msg, context);
            break;
        }
    return false;
    }

static bool handleSetPinMode(int size, const byte *msg, CONTEXT *context)
    {
    byte *expr = (byte *) &msg[1];
    byte pinNo = evalWord8Expr(&expr, context);
    pinMode(pinNo, *expr);
    return false;
    }

static bool handleDelayMillis(int size, const byte *msg, CONTEXT *context)
    {
    byte *expr = (byte *) &msg[2];
    uint32_t millis = evalWord32Expr(&expr, context);

    if (context->task && !isCodeBlock())
        {
        delayRunningTask(millis);
        }
    else 
        {
        delay(millis);
        if (!context->task && !isCodeBlock())
            {
            sendReply(0, BC_RESP_DELAY, NULL, NULL,0);
            }
        }
    return true;
    }

static bool handleDelayMicros(int size, const byte *msg, CONTEXT *context)
    {
    byte *expr = (byte *) &msg[2];
    uint32_t micros = evalWord16Expr(&expr, context);
    delayMicroseconds(micros);
    if (!context->task && !isCodeBlock())
        {
        sendReply(0, BC_RESP_DELAY, NULL, NULL,0);
        }
    return false;
    }

static bool handleSystemReset(int size, const byte *msg, CONTEXT *context)
    {
    soft_restart();
    return false;
    }

static bool handleLoop(int size, const byte *msg, CONTEXT *context)
    {
    byte *codeBlock = (byte *) &msg[1];
    int loopSize = size - 1;

    while (true)
        {
        runCodeBlock(loopSize, codeBlock, context);
        }

    return false;
    }

static bool handleWhile(int size, const byte *msg, CONTEXT *context)
    {
    byte *expr = (byte *) &msg[1];
    byte refIndex = evalWord8Expr(&expr, context);
    byte *condExpr = expr;
    bool condition = evalBoolExpr(&expr, context);
    byte updateLength = *expr++;
    byte *updateExpr = expr;
    byte exprType = *updateExpr >> EXPR_TYPE_SHFT;
    byte *codeBlock = (expr + updateLength);
    int whileSize = size - (expr + updateLength - msg);

    while (condition)
        {
        runCodeBlock(whileSize, codeBlock, context);

        expr = updateExpr;
        switch (exprType)
            {
            case EXPR_BOOL:
                storeBoolRef(expr, context, refIndex);
                break;
            case EXPR_WORD8:
                storeWord8Ref(expr, context, refIndex);
                break;
            case EXPR_WORD16:
                storeWord16Ref(expr, context, refIndex);
                break;
            case EXPR_WORD32:
                storeWord32Ref(expr, context, refIndex);
                break;
            case EXPR_LIST8:
                storeList8Ref(expr, context, refIndex);
                break;
            }
        expr = condExpr;
        condition = evalBoolExpr(&expr, context);
        }

    return false;
    }

static bool handleIfThenElse(int size, const byte *msg, CONTEXT *context)
    {
    uint16_t thenSize, elseSize;
    memcpy(&thenSize, &msg[1], sizeof(thenSize));
    byte *expr = (byte *) &msg[3];
    bool condition = evalBoolExpr(&expr, context);
    byte *codeBlock = expr;

    if (condition)
        {
        runCodeBlock(thenSize, codeBlock, context);
        }
    else
        {
        elseSize = size - (thenSize + (codeBlock - msg));
        runCodeBlock(elseSize, codeBlock + thenSize, context);
        }

    return false;
    }

