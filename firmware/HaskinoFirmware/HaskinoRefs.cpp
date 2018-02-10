#include <Arduino.h>
#include "HaskinoCodeBlock.h"
#include "HaskinoComm.h"
#include "HaskinoCommands.h"
#include "HaskinoConfig.h"
#include "HaskinoExpr.h"
#include "HaskinoRefs.h"

static void  *haskinoRefs[MAX_REFS];

static bool handleNewRef(int type, int size, const byte *msg, CONTEXT *context);
static bool handleReadRef(int type, int size, const byte *msg, CONTEXT *context);
static bool handleWriteRef(int type, int size, const byte *msg, CONTEXT *context);

bool parseRefMessage(int size, const byte *msg, CONTEXT *context)
    {
    byte cmd = msg[0];
    byte type = msg[1];

    switch (cmd) 
        {
        case REF_CMD_NEW:
            handleNewRef(type, size, msg, context);
            break;
        case REF_CMD_READ:
            handleReadRef(type, size, msg, context);
            break;
        case REF_CMD_WRITE:
            handleWriteRef(type, size, msg, context);
            break;
        }
    return false;
    }

bool readRefBool(int refIndex)
    {
    return (bool) haskinoRefs[refIndex];
    }

uint8_t readRefWord8(int refIndex)
    {
    return (uint8_t) (uint32_t) haskinoRefs[refIndex];
    }

uint16_t readRefWord16(int refIndex)
    {
    return (uint16_t) haskinoRefs[refIndex];
    }

uint32_t readRefWord32(int refIndex)
    {
    return (uint32_t) haskinoRefs[refIndex];
    }

int8_t readRefInt8(int refIndex)
    {
    return (int8_t) (uint32_t) haskinoRefs[refIndex];
    }

int16_t readRefInt16(int refIndex)
    {
    return (int16_t) haskinoRefs[refIndex];
    }

int32_t readRefInt32(int refIndex)
    {
    return (int32_t) haskinoRefs[refIndex];
    }

uint8_t *readRefList8(int refIndex)
    {
    return (uint8_t *) haskinoRefs[refIndex];
    }

float readRefFloat(int refIndex)
    {
    float fVal;
    
    memcpy(&fVal, (byte *) &haskinoRefs[refIndex], sizeof(float));
    return fVal;
    }

void storeBoolRef(byte *expr, CONTEXT *context, byte refIndex)
    {
    bool bVal = evalBoolExpr(&expr, context);

    haskinoRefs[refIndex] = (void *) bVal;
    }

void storeWord8Ref(byte *expr, CONTEXT *context, byte refIndex)
    {
    uint8_t w8Val = evalWord8Expr(&expr, context);

    haskinoRefs[refIndex] = (void *) (uint32_t) w8Val;
    }

void storeWord16Ref(byte *expr, CONTEXT *context, byte refIndex)
    {
    uint16_t w16Val = evalWord16Expr(&expr, context);

    haskinoRefs[refIndex] = (void *) (uint32_t) w16Val;
    }

void storeWord32Ref(byte *expr, CONTEXT *context, byte refIndex)
    {
    uint32_t w32Val = evalWord32Expr(&expr, context);

    haskinoRefs[refIndex] = (void *) w32Val;
    }

void storeInt8Ref(byte *expr, CONTEXT *context, byte refIndex)
    {
    int8_t i8Val = evalInt8Expr(&expr, context);

    haskinoRefs[refIndex] = (void *) (int32_t) i8Val;
    }

void storeInt16Ref(byte *expr, CONTEXT *context, byte refIndex)
    {
    int16_t i16Val = evalInt16Expr(&expr, context);

    haskinoRefs[refIndex] = (void *) (int32_t) i16Val;
    }

void storeInt32Ref(byte *expr, CONTEXT *context, byte refIndex)
    {
    int32_t i32Val = evalInt32Expr(&expr, context);

    haskinoRefs[refIndex] = (void *) i32Val;
    }

void storeFloatRef(byte *expr, CONTEXT *context, byte refIndex)
    {
    float fVal = evalFloatExpr(&expr, context);

    memcpy(&haskinoRefs[refIndex], (byte *) &fVal, sizeof(float));
    }

void storeList8Ref(byte *expr, CONTEXT *context, byte refIndex)
    {
    bool alloc;
    byte *lVal = evalList8Expr(&expr, context, &alloc);

    if (haskinoRefs[refIndex] != NULL)
        {
        free(haskinoRefs[refIndex]);
        haskinoRefs[refIndex] = NULL;
        }

    if (alloc)
        haskinoRefs[refIndex] = lVal;
    else
        {
        // Need to copy expression
        byte *newLVal = (byte *) malloc(lVal[1]+2);
        if (newLVal)
            {
            memcpy(newLVal, lVal, lVal[1]+2);
            haskinoRefs[refIndex] = newLVal;
            }
        }    
    }

static bool handleNewRef(int type, int size, const byte *msg, CONTEXT *context)
    {
    byte bind = msg[2];
    byte refIndex = msg[3];
    byte *expr = (byte *) &msg[4];
    byte newReply[2];

    // ToDo: Handle overflow of ref numbers

    if (refIndex >= MAX_REFS)
        {
        sendReply(0, REF_RESP_NEW, NULL, context, bind);
        }
    else
        {
        newReply[1] = EXPR_LIT;
        switch (type)
            {
            case EXPR_BOOL:
                newReply[0] = EXPR_BOOL;
                storeBoolRef(expr, context, refIndex);
                break;
            case EXPR_WORD8:
                newReply[0] = EXPR_WORD8;
                storeWord8Ref(expr, context, refIndex);
                break;
            case EXPR_WORD16:
                newReply[0] = EXPR_WORD16;
                storeWord16Ref(expr, context, refIndex);
                break;
            case EXPR_WORD32:
                newReply[0] = EXPR_WORD32;
                storeWord32Ref(expr, context, refIndex);
                break;
            case EXPR_LIST8:
                newReply[0] = EXPR_LIST8;
                storeList8Ref(expr, context, refIndex);
                break;
            case EXPR_INT8:
                newReply[0] = EXPR_INT8;
                storeInt8Ref(expr, context, refIndex);
                break;
            case EXPR_INT16:
                newReply[0] = EXPR_INT16;
                storeInt16Ref(expr, context, refIndex);
                break;
            case EXPR_INT32:
                newReply[0] = EXPR_INT32;
                storeInt32Ref(expr, context, refIndex);
                break;
            case EXPR_FLOAT:
                newReply[0] = EXPR_FLOAT;
                storeFloatRef(expr, context, refIndex);
                break;
            }
        newReply[2] = refIndex;
        sendReply(sizeof(byte)+2, REF_RESP_NEW, newReply, context, bind);
        }
    return false;
    }

static bool handleReadRef(int type, int size, const byte *msg, CONTEXT *context)
    {
    byte bind = msg[2];
    byte *expr = (byte *) &msg[3];
    byte refIndex = evalWord8Expr(&expr, context);
    byte readReply[6];
    byte *lVal;

    readReply[0] = type;
    readReply[1] = EXPR_LIT;
    // ToDo:  Check for param errors
    switch (type)
        {
        case EXPR_BOOL:
            readReply[2] = (bool) haskinoRefs[refIndex];
            sendReply(sizeof(bool)+2, REF_RESP_READ, readReply, context, bind);
            break;
        case EXPR_WORD8:
            readReply[2] = (uint8_t) (uint32_t) haskinoRefs[refIndex];
            sendReply(sizeof(uint8_t)+2, REF_RESP_READ, readReply, context, bind);
            break;
        case EXPR_WORD16:
            memcpy(&readReply[2], (byte *) &haskinoRefs[refIndex], sizeof(uint16_t));
            sendReply(sizeof(uint16_t)+2, REF_RESP_READ, readReply, context, bind);
            break;
        case EXPR_WORD32:
            memcpy(&readReply[2], (byte *) &haskinoRefs[refIndex], sizeof(uint32_t));
            sendReply(sizeof(uint32_t)+2, REF_RESP_READ, readReply, context, bind);
            break;
        case EXPR_INT8:
            readReply[2] = (int8_t) (int32_t) haskinoRefs[refIndex];
            sendReply(sizeof(int8_t)+2, REF_RESP_READ, readReply, context, bind);
            break;
        case EXPR_INT16:
            memcpy(&readReply[2], (byte *) &haskinoRefs[refIndex], sizeof(int16_t));
            sendReply(sizeof(int16_t)+2, REF_RESP_READ, readReply, context, bind);
            break;
        case EXPR_INT32:
            memcpy(&readReply[2], (byte *) &haskinoRefs[refIndex], sizeof(int32_t));
            sendReply(sizeof(int32_t)+2, REF_RESP_READ, readReply, context, bind);
            break;
        case EXPR_LIST8:
            lVal = (byte *) haskinoRefs[refIndex];
            sendReply(lVal[2]+3, REF_RESP_READ, lVal, context, bind);
            break;
        case EXPR_FLOAT:
            memcpy(&readReply[2], (byte *) &haskinoRefs[refIndex], sizeof(float));
            sendReply(sizeof(float)+2, REF_RESP_READ, readReply, context, bind);
            break;
        }
    return false;
    }

static bool handleWriteRef(int type, int size, const byte *msg, CONTEXT *context)
    {
    byte *expr = (byte *) &msg[2];
    byte refIndex = evalWord8Expr(&expr, context);

    // ToDo:  Check for param errors

    switch (type)
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
        case EXPR_INT8:
            storeInt8Ref(expr, context, refIndex);
            break;
        case EXPR_INT16:
            storeInt16Ref(expr, context, refIndex);
            break;
        case EXPR_INT32:
            storeInt32Ref(expr, context, refIndex);
            break;
        case EXPR_LIST8:
            storeList8Ref(expr, context, refIndex);
            break;
        case EXPR_FLOAT:
            storeFloatRef(expr, context, refIndex);
            break;
        }
    return false;
    }
