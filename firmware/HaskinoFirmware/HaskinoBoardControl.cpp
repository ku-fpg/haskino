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
static bool handleForIn(int size, const byte *msg, CONTEXT *context);

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
        case BC_CMD_FORIN:
            return handleForIn(size, msg, context);
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
    byte bind = msg[1];

    uint32_t millis = evalWord32Expr(&expr, context);

    if (context->task)
        {
        delayRunningTask(millis);
        return true;
        }
    else 
        {
        delay(millis);
        sendReply(0, BC_RESP_DELAY, NULL, context, bind);
        return false;
        }
    }

static bool handleDelayMicros(int size, const byte *msg, CONTEXT *context)
    {
    byte *expr = (byte *) &msg[2];
    byte bind = msg[1];

    uint32_t micros = evalWord16Expr(&expr, context);
    delayMicroseconds(micros);
    sendReply(0, BC_RESP_DELAY, NULL, context, bind);
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
    bool rescheduled;

    while (true)
        {
        rescheduled = runCodeBlock(loopSize, codeBlock, context);
        if (rescheduled)
            return true;
        }

    return false;
    }

static bool handleForIn(int size, const byte *msg, CONTEXT *context)
    {
    byte *expr = (byte *) &msg[1];
    bool alloc;
    byte *listPtr = evalList8Expr(&expr, context, &alloc);
    byte listSize = listPtr[1];
    byte bindIndex = expr[1];
    byte *codeBlock = expr + 2;
    int forSize = size - (codeBlock - msg);
    bool rescheduled = context->task && context->task->rescheduled;
    int i;

    context->bind[bindIndex * BIND_SPACING] = EXPR(EXPR_WORD8, EXPR_LIT);
    for (i=rescheduled ? 
            context->blockStatus[context->recallBlockLevel+1].info.index : 0;
         i<listSize;
         i++)
        {
        context->bind[bindIndex * BIND_SPACING + 1] = listPtr[i+2];
        context->blockStatus[context->currBlockLevel+1].info.index = i;
        rescheduled = runCodeBlock(forSize, codeBlock, context);
        if (rescheduled)
            {
            if (alloc) 
                free(listPtr);

            return true;
            }
        }

    if (alloc) 
        free(listPtr);

    return false;
    }

static bool handleWhile(int size, const byte *msg, CONTEXT *context)
    {
    byte bind = msg[1];
    byte *expr = (byte *) &msg[2];
    bool rescheduled = (context->task && context->task->rescheduled);
    byte initLength;
    byte *initExpr;
    byte *condExpr;
    bool condition;
    byte exprType;
    byte *codeBlock;
    int whileSize;
    byte whileReply[5];
    
    initLength = *expr++;
    initExpr = expr;
    exprType = *initExpr >> EXPR_TYPE_SHFT;
    expr += initLength;
    condExpr = expr;

    if (!rescheduled)
        {
        switch (exprType)
            {
            case REF_BOOL:
                storeBoolBind(initExpr, context, bind);
                break;
            case REF_WORD8:
                storeWord8Bind(initExpr, context, bind);
                break;
            case REF_WORD16:
                storeWord16Bind(initExpr, context, bind);
                break;
            case REF_WORD32:
                storeWord32Bind(initExpr, context, bind);
                break;
            case REF_INT8:
                storeInt8Bind(initExpr, context, bind);
                break;
            case REF_INT16:
                storeInt16Bind(initExpr, context, bind);
                break;
            case REF_INT32:
                storeInt32Bind(initExpr, context, bind);
                break;
            case REF_LIST8:
                storeList8Bind(initExpr, context, bind);
                break;
            case REF_FLOAT:
                storeFloatBind(initExpr, context, bind);
                break;
            }
        }

    condition = evalBoolExpr(&expr, context);
    codeBlock = expr;
    whileSize = size - (expr - msg);

    // If we were rescheduled, always enter the loop to rerun code block
    while (condition || rescheduled)
        {
        rescheduled = runCodeBlock(whileSize, codeBlock, context);
        if (rescheduled) {
             return true;
        }

        expr = condExpr;
        condition = evalBoolExpr(&expr, context);
        }

    sendTypeReply(exprType, &context->bind[bind * BIND_SPACING], whileReply, 
                  BC_RESP_WHILE, context, bind);

    return false;
}

static bool handleIfThenElse(int size, const byte *msg, CONTEXT *context)
    {
    byte type = msg[1];
    byte bind = msg[2];
    uint16_t thenSize, elseSize;
    memcpy(&thenSize, &msg[3], sizeof(thenSize));
    byte *expr = (byte *) &msg[5];
    bool condition = evalBoolExpr(&expr, context);
    byte *codeBlock = expr;
    bool test;
    bool rescheduled = context->task && context->task->rescheduled;
    byte ifReply[5];

    if (rescheduled)
        {
        // get test value from block status
        test = context->blockStatus[context->recallBlockLevel+1].info.condition;
        }
    else
        {
        test = condition;
        // save condition to block status
        context->blockStatus[context->currBlockLevel+1].info.condition = condition;
        }

    if (test)
        {
        rescheduled = runCodeBlock(thenSize, codeBlock, context);
        }
    else
        {
        elseSize = size - (thenSize + (codeBlock - msg));
        rescheduled = runCodeBlock(elseSize, codeBlock + thenSize, context);
        }

    if (!rescheduled && type != REF_UNIT)
        sendTypeReply(type, &context->bind[bind * BIND_SPACING], ifReply, 
                      BC_RESP_IF_THEN_ELSE, context, bind);

    return rescheduled;
    }

