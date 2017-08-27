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
static bool handleIterate(int size, const byte *msg, CONTEXT *context);
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
        case BC_CMD_ITERATE:
            return handleIterate(size, msg, context);
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
    byte value = evalWord8Expr(&expr, context);

    pinMode(pinNo, value);
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

static int typeToSize(int type, byte *src)
    {
    switch(type)
        {
        case EXPR_UNIT:
            return 2;
        case EXPR_WORD16:
        case EXPR_INT16:
            return 4;
        case EXPR_BOOL:
        case EXPR_WORD8:
        case EXPR_INT8:
            return 3;
        case EXPR_LIST8:
            return src[2] + 3;
        case EXPR_WORD32:
        case EXPR_INT32:
        case EXPR_FLOAT:
        default:
            return 6;
        }
    } 

static bool handleIterate(int size, const byte *msg, CONTEXT *context)
    {
    byte type1 = msg[1];
    byte type2 = msg[2];
    byte bind = msg[3];
    byte *expr = (byte *) &msg[4];
    bool rescheduled = (context->task && context->task->rescheduled);
    byte *bind_ptr = &context->bind[bind * BIND_SPACING];
    byte initLength;
    byte *initExpr;
    bool condition;
    byte *codeBlock;
    int iterSize;
    
    initLength = *expr++;
    // ToDo:  Verify length for long inits.
    initExpr = expr;
    expr += initLength;

    if (!rescheduled)
        {
        switch (type1)
            {
            case EXPR_UNIT:
                storeUnitBind(initExpr, context, bind);
                break;
            case EXPR_BOOL:
                storeBoolBind(initExpr, context, bind);
                break;
            case EXPR_WORD8:
                storeWord8Bind(initExpr, context, bind);
                break;
            case EXPR_WORD16:
                storeWord16Bind(initExpr, context, bind);
                break;
            case EXPR_WORD32:
                storeWord32Bind(initExpr, context, bind);
                break;
            case EXPR_INT8:
                storeInt8Bind(initExpr, context, bind);
                break;
            case EXPR_INT16:
                storeInt16Bind(initExpr, context, bind);
                break;
            case EXPR_INT32:
                storeInt32Bind(initExpr, context, bind);
                break;
            case EXPR_LIST8:
                storeList8Bind(initExpr, context, bind);
                break;
            case EXPR_FLOAT:
                storeFloatBind(initExpr, context, bind);
                break;
            }
        }

    codeBlock = expr;
    iterSize = size - (expr - msg);
    condition = true;

    // If we were rescheduled, always enter the loop to rerun code block
    while (condition || rescheduled)
        {
        rescheduled = runCodeBlock(iterSize, codeBlock, context);
        if (rescheduled) {
             return true;
        }

        condition = ((*bind_ptr & EXPRE_LEFT_FLAG) == EXPRE_LEFT_FLAG);
        }

    sendReply( typeToSize(type2, &context->bind[bind * BIND_SPACING]),
               BC_RESP_ITERATE, &context->bind[bind * BIND_SPACING],
               context, bind);
    return false;
}

static bool handleIfThenElse(int size, const byte *msg, CONTEXT *context)
    {
    byte type;
    byte type1 = msg[1];
    byte type2 = msg[2];
    byte bind = msg[3];
    uint16_t thenSize, elseSize;
    memcpy(&thenSize, &msg[4], sizeof(thenSize));
    byte *expr = (byte *) &msg[6];
    bool condition = evalBoolExpr(&expr, context);
    byte *codeBlock = expr;
    bool test;
    bool rescheduled = context->task && context->task->rescheduled;
    byte *bind_ptr = &context->bind[bind * BIND_SPACING];
    byte ifReply[6];

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

    if ((*bind_ptr & EXPRE_LEFT_FLAG) == EXPRE_LEFT_FLAG)
        type = type1;
    else
        type = type2;

    if (!rescheduled)
        sendReply( typeToSize(type, &context->bind[bind * BIND_SPACING]),
                   BC_RESP_IF_THEN_ELSE, &context->bind[bind * BIND_SPACING],
                   context, bind);


    return rescheduled;
    }

