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
    byte *expr = (byte *) &msg[1];
    byte refIndex = evalWord8Expr(&expr, context);
    byte *condExpr = expr;
    bool condition = evalBoolExpr(&expr, context);
    byte updateLength = *expr++;
    byte *updateExpr = expr;
    byte exprType = *updateExpr >> EXPR_TYPE_SHFT;
    byte *codeBlock = (expr + updateLength);
    int whileSize = size - (expr + updateLength - msg);
    bool rescheduled = (context->task && context->task->rescheduled);
    
    // If we were rescheduled, always enter the loop to rerun code block
    while (condition || rescheduled)
        {
        rescheduled = runCodeBlock(whileSize, codeBlock, context);
        if (rescheduled) {
             return true;
        }

        expr = updateExpr;
        switch (exprType)
            {
            case REF_BOOL:
                storeBoolRef(expr, context, refIndex);
                break;
            case REF_WORD8:
                storeWord8Ref(expr, context, refIndex);
                break;
            case REF_WORD16:
                storeWord16Ref(expr, context, refIndex);
                break;
            case REF_WORD32:
                storeWord32Ref(expr, context, refIndex);
                break;
            case REF_INT8:
                storeInt8Ref(expr, context, refIndex);
                break;
            case REF_INT16:
                storeInt16Ref(expr, context, refIndex);
                break;
            case REF_INT32:
                storeInt32Ref(expr, context, refIndex);
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
    byte type = msg[1];
    byte bind = msg[2];
    uint16_t thenSize, elseSize;
    memcpy(&thenSize, &msg[3], sizeof(thenSize));
    byte *expr = (byte *) &msg[4];
    bool condition = evalBoolExpr(&expr, context);
    byte *codeBlock = expr;
    bool test;
    bool rescheduled = context->task && context->task->rescheduled;
    byte ifReply[2];
    void *resultPtr;
    byte *lVal;

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

    resultPtr = &context->bind[bind * BIND_SPACING];

    switch (type)
        {
        case REF_BOOL:
            ifReply[0] = EXPR(EXPR_BOOL, EXPR_LIT);
            ifReply[1] = *((bool *) resultPtr);
            sendReply(sizeof(bool)+1, EXPR_RESP_RET, ifReply, context, bind);
            break;
        case REF_WORD8:
            ifReply[0] = EXPR(EXPR_WORD8, EXPR_LIT);
            ifReply[1] = *((uint8_t *) resultPtr);
            sendReply(sizeof(uint8_t)+1, EXPR_RESP_RET, ifReply, context, bind);
            break;
        case REF_WORD16:
            ifReply[0] = EXPR(EXPR_WORD16, EXPR_LIT);
            memcpy(&ifReply[1], (byte *) resultPtr, sizeof(uint16_t));
            sendReply(sizeof(uint16_t)+1, EXPR_RESP_RET, ifReply, context, bind);
            break;
        case REF_WORD32:
            ifReply[0] = EXPR(EXPR_WORD32, EXPR_LIT);
            memcpy(&ifReply[1], (byte *) resultPtr, sizeof(uint32_t));
            sendReply(sizeof(uint32_t)+1, EXPR_RESP_RET, ifReply, context, bind);
            break;
        case REF_INT8:
            ifReply[0] = EXPR(EXPR_INT8, EXPR_LIT);
            ifReply[1] = *((int8_t *) resultPtr);
            sendReply(sizeof(int8_t)+1, EXPR_RESP_RET, ifReply, context, bind);
            break;
        case REF_INT16:
            ifReply[0] = EXPR(EXPR_INT16, EXPR_LIT);
            memcpy(&ifReply[1], (byte *) resultPtr, sizeof(int16_t));
            sendReply(sizeof(int16_t)+1, EXPR_RESP_RET, ifReply, context, bind);
            break;
        case REF_INT32:
            ifReply[0] = EXPR(EXPR_INT32, EXPR_LIT);
            memcpy(&ifReply[1], (byte *) resultPtr, sizeof(int32_t));
            sendReply(sizeof(int32_t)+1, EXPR_RESP_RET, ifReply, context, bind);
            break;
        case REF_LIST8:
            lVal = (byte *) resultPtr;
            sendReply(lVal[1]+2, EXPR_RESP_RET, lVal, context, bind);
            break;
        case REF_FLOAT:
            ifReply[0] = EXPR_F(EXPR_LIT);
            memcpy(&ifReply[1], (byte *) resultPtr, sizeof(float));
            sendReply(sizeof(float)+1, EXPR_RESP_RET, ifReply, context, bind);
            break;
        default:
            break;
        }

    return rescheduled;
    }

