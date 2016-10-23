#include <Arduino.h>
#include "HaskinoCodeBlock.h"
#include "HaskinoComm.h"
#include "HaskinoCommands.h"
#include "HaskinoConfig.h"
#include "HaskinoExpr.h"
#include "HaskinoRefs.h"

typedef struct ref_record {
    void *ref;
    byte type;
    byte len;   // Number of type elements in ref
} REF_RECORD;

static REF_RECORD haskinoRefs[MAX_REFS];

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
    if (haskinoRefs[refIndex].ref != NULL)
        return *((bool *) haskinoRefs[refIndex].ref);
    else
        {
#ifdef DEBUG
        sendStringf("rRB: %d", refIndex);
#endif
        return false;
        }
    }

uint8_t readRefWord8(int refIndex)
    {
    if (haskinoRefs[refIndex].ref != NULL)
        return *((uint8_t *) haskinoRefs[refIndex].ref);
    else
        {
#ifdef DEBUG
        sendStringf("rRW8: %d", refIndex);
#endif
        return false;
        }
    }

uint16_t readRefWord16(int refIndex)
    {
    if (haskinoRefs[refIndex].ref != NULL)
        return *((uint16_t *) haskinoRefs[refIndex].ref);
    else
        {
#ifdef DEBUG
        sendStringf("rRW16: %d", refIndex);
#endif
        return false;
        }
    }

uint32_t readRefWord32(int refIndex)
    {
    if (haskinoRefs[refIndex].ref != NULL)
        return *((uint32_t *) haskinoRefs[refIndex].ref);
    else
        {
#ifdef DEBUG
        sendStringf("rRW32: %d", refIndex);
#endif
        return false;
        }
    }

int8_t readRefInt8(int refIndex)
    {
    if (haskinoRefs[refIndex].ref != NULL)
        return *((int8_t *) haskinoRefs[refIndex].ref);
    else
        {
#ifdef DEBUG
        sendStringf("rRI8: %d", refIndex);
#endif
        return false;
        }
    }

int16_t readRefInt16(int refIndex)
    {
    if (haskinoRefs[refIndex].ref != NULL)
        return *((int16_t *) haskinoRefs[refIndex].ref);
    else
        {
#ifdef DEBUG
        sendStringf("rRI16: %d", refIndex);
#endif
        return false;
        }
    }

int32_t readRefInt32(int refIndex)
    {
    if (haskinoRefs[refIndex].ref != NULL)
        return *((int32_t *) haskinoRefs[refIndex].ref);
    else
        {
#ifdef DEBUG
        sendStringf("rRI32: %d", refIndex);
#endif
        return false;
        }
    }

uint8_t *readRefList8(int refIndex)
    {
    if (haskinoRefs[refIndex].ref != NULL)
        return (uint8_t *) haskinoRefs[refIndex].ref;
    else
        {
#ifdef DEBUG
        sendStringf("rRL8: %d", refIndex);
#endif
        return false;
        }
    }

float readRefFloat(int refIndex)
    {
    if (haskinoRefs[refIndex].ref != NULL)
        return *((float *) haskinoRefs[refIndex].ref);
    else
        {
#ifdef DEBUG
        sendStringf("rRF: %d", refIndex);
#endif
        return false;
        }
    }

static int typeToSize(int type)
    {
    switch(type)
        {
        case REF_BOOL:
        case REF_WORD16:
        case REF_INT16:
            return 2;
        case REF_WORD8:
        case REF_INT8:
            return 1;
        case REF_WORD32:
        case REF_INT32:
        case REF_FLOAT:
        default:
            return 4;
        }
    } 

byte *storeBoolRef(byte *expr, CONTEXT *context, byte refIndex, bool store)
    {
    bool bVal = evalBoolExpr(&expr, context);

    if (store)
        *((bool *) haskinoRefs[refIndex].ref) = bVal;
    return expr;
    }

byte *storeWord8Ref(byte *expr, CONTEXT *context, byte refIndex, bool store)
    {
    uint8_t w8Val = evalWord8Expr(&expr, context);

    if (store)
        *((uint8_t *) haskinoRefs[refIndex].ref) = w8Val;
    return expr;
    }

byte *storeWord16Ref(byte *expr, CONTEXT *context, byte refIndex, bool store)
    {
    uint16_t w16Val = evalWord16Expr(&expr, context);

    if (store)
        *((uint16_t *) haskinoRefs[refIndex].ref) = w16Val;
    return expr;
    }

byte *storeWord32Ref(byte *expr, CONTEXT *context, byte refIndex, bool store)
    {
    uint32_t w32Val = evalWord32Expr(&expr, context);

    if (store)
        *((uint32_t *) haskinoRefs[refIndex].ref) = w32Val;
    return expr;
    }

byte *storeInt8Ref(byte *expr, CONTEXT *context, byte refIndex, bool store)
    {
    int8_t i8Val = evalInt8Expr(&expr, context);

    if (store)
        *((int8_t *) haskinoRefs[refIndex].ref) = i8Val;
    return expr;
    }

byte *storeInt16Ref(byte *expr, CONTEXT *context, byte refIndex, bool store)
    {
    int16_t i16Val = evalInt16Expr(&expr, context);

    if (store)
    *((int16_t *) haskinoRefs[refIndex].ref) = i16Val;
    return expr;
    }

byte *storeInt32Ref(byte *expr, CONTEXT *context, byte refIndex, bool store)
    {
    int32_t i32Val = evalInt32Expr(&expr, context);

    if (store)
        *((int32_t *) haskinoRefs[refIndex].ref) = i32Val;
    return expr;
    }

byte *storeFloatRef(byte *expr, CONTEXT *context, byte refIndex, bool store)
    {
    float fVal = evalFloatExpr(&expr, context);

    if (store)
        *((float *) haskinoRefs[refIndex].ref) = fVal;
    return expr;
    }

byte *storeList8Ref(byte *expr, CONTEXT *context, byte refIndex, bool store)
    {
    bool alloc;
    byte *lVal = evalList8Expr(&expr, context, &alloc);

    if (store)
        {
        if (haskinoRefs[refIndex].ref != NULL)
            {
            free(haskinoRefs[refIndex].ref);
            haskinoRefs[refIndex].ref = NULL;
            }

        if (alloc)
            haskinoRefs[refIndex].ref = lVal;
        else
            {
            // Need to copy expression
            byte *newLVal = (byte *) malloc(lVal[1]+2);
            if (newLVal)
                {
                memcpy(newLVal, lVal, lVal[1]+2);
                haskinoRefs[refIndex].ref = newLVal;
                }
            }    
        }
    return expr;
    }

static bool handleNewRef(int type, int size, const byte *msg, CONTEXT *context)
    {
    byte bind = msg[2];
    int count = 1;
    int spaceNeeded = typeToSize(type) * count;
    byte refIndex = msg[3];
    byte *expr = (byte *) &msg[4];
    void *memory;
    byte newReply[2];

    // ToDo: Handle overflow of ref numbers

    if ((refIndex >= MAX_REFS) ||
        (((type != REF_LIST8) && (memory = malloc(spaceNeeded)) == NULL)))
        {
        sendReply(0, REF_RESP_NEW, NULL, context, bind);
        }
    else
        {
        if (type != REF_LIST8)
            {
            if (haskinoRefs[refIndex].ref != NULL)
                {
                free(haskinoRefs[refIndex].ref);
                }
            haskinoRefs[refIndex].ref = memory;
            }
        haskinoRefs[refIndex].type = type;
        haskinoRefs[refIndex].len = count;
        switch (type)
            {
            case REF_BOOL:
                newReply[0] = EXPR(EXPR_BOOL, EXPR_LIT);
                storeBoolRef(expr, context, refIndex, true);
                break;
            case REF_WORD8:
                newReply[0] = EXPR(EXPR_WORD8, EXPR_LIT);
                storeWord8Ref(expr, context, refIndex, true);
                break;
            case REF_WORD16:
                newReply[0] = EXPR(EXPR_WORD16, EXPR_LIT);
                storeWord16Ref(expr, context, refIndex, true);
                break;
            case REF_WORD32:
                newReply[0] = EXPR(EXPR_WORD32, EXPR_LIT);
                storeWord32Ref(expr, context, refIndex, true);
                break;
            case REF_LIST8:
                newReply[0] = EXPR_L(EXPR_LIT);
                storeList8Ref(expr, context, refIndex, true);
                break;
            case REF_INT8:
                newReply[0] = EXPR(EXPR_INT8, EXPR_LIT);
                storeInt8Ref(expr, context, refIndex, true);
                break;
            case REF_INT16:
                newReply[0] = EXPR(EXPR_INT16, EXPR_LIT);
                storeInt16Ref(expr, context, refIndex, true);
                break;
            case REF_INT32:
                newReply[0] = EXPR(EXPR_INT32, EXPR_LIT);
                storeInt32Ref(expr, context, refIndex, true);
                break;
            case REF_FLOAT:
                newReply[0] = EXPR_F(EXPR_LIT);
                storeFloatRef(expr, context, refIndex, true);
                break;
            }
        newReply[1] = refIndex;
        sendReply(sizeof(byte)+1, REF_RESP_NEW, newReply, context, bind);
        }
    return false;
    }

static bool handleReadRef(int type, int size, const byte *msg, CONTEXT *context)
    {
    byte bind = msg[2];
    byte *expr = (byte *) &msg[3];
    byte refIndex = evalWord8Expr(&expr, context);
    byte readReply[5];
    byte *lVal;

    // ToDo:  Check for param errors
    switch (type)
        {
        case REF_BOOL:
            readReply[0] = EXPR(EXPR_BOOL, EXPR_LIT);
            readReply[1] = *((bool *) haskinoRefs[refIndex].ref);
            sendReply(sizeof(bool)+1, REF_RESP_READ, readReply, context, bind);
            break;
        case REF_WORD8:
            readReply[0] = EXPR(EXPR_WORD8, EXPR_LIT);
            readReply[1] = *((uint8_t *) haskinoRefs[refIndex].ref);
            sendReply(sizeof(uint8_t)+1, REF_RESP_READ, readReply, context, bind);
            break;
        case REF_WORD16:
            readReply[0] = EXPR(EXPR_WORD16, EXPR_LIT);
            memcpy(&readReply[1], (byte *) haskinoRefs[refIndex].ref, sizeof(uint16_t));
            sendReply(sizeof(uint16_t)+1, REF_RESP_READ, readReply, context, bind);
            break;
        case REF_WORD32:
            readReply[0] = EXPR(EXPR_WORD32, EXPR_LIT);
            memcpy(&readReply[1], (byte *) haskinoRefs[refIndex].ref, sizeof(uint32_t));
            sendReply(sizeof(uint32_t)+1, REF_RESP_READ, readReply, context, bind);
            break;
        case REF_INT8:
            readReply[0] = EXPR(EXPR_INT8, EXPR_LIT);
            readReply[1] = *((int8_t *) haskinoRefs[refIndex].ref);
            sendReply(sizeof(int8_t)+1, REF_RESP_READ, readReply, context, bind);
            break;
        case REF_INT16:
            readReply[0] = EXPR(EXPR_INT16, EXPR_LIT);
            memcpy(&readReply[1], (byte *) haskinoRefs[refIndex].ref, sizeof(int16_t));
            sendReply(sizeof(int16_t)+1, REF_RESP_READ, readReply, context, bind);
            break;
        case REF_INT32:
            readReply[0] = EXPR(EXPR_INT32, EXPR_LIT);
            memcpy(&readReply[1], (byte *) haskinoRefs[refIndex].ref, sizeof(int32_t));
            sendReply(sizeof(int32_t)+1, REF_RESP_READ, readReply, context, bind);
            break;
        case REF_LIST8:
            lVal = (byte *) haskinoRefs[refIndex].ref;
            sendReply(lVal[1]+2, REF_RESP_READ, lVal, context, bind);
            break;
        case REF_FLOAT:
            readReply[0] = EXPR_F(EXPR_LIT);
            memcpy(&readReply[1], (byte *) haskinoRefs[refIndex].ref, sizeof(float));
            sendReply(sizeof(float)+1, REF_RESP_READ, readReply, context, bind);
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
        case REF_BOOL:
            storeBoolRef(expr, context, refIndex, true);
            break;
        case REF_WORD8:
            storeWord8Ref(expr, context, refIndex, true);
            break;
        case REF_WORD16:
            storeWord16Ref(expr, context, refIndex, true);
            break;
        case REF_WORD32:
            storeWord32Ref(expr, context, refIndex, true);
            break;
        case REF_INT8:
            storeInt8Ref(expr, context, refIndex, true);
            break;
        case REF_INT16:
            storeInt16Ref(expr, context, refIndex, true);
            break;
        case REF_INT32:
            storeInt32Ref(expr, context, refIndex, true);
            break;
        case REF_LIST8:
            storeList8Ref(expr, context, refIndex, true);
            break;
        case REF_FLOAT:
            storeFloatRef(expr, context, refIndex, true);
            break;
        }
    return false;
    }
