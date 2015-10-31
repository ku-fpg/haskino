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
        sendStringf("readRefBool: Invalid ref index %d", refIndex);
        return false;
        }
    }

uint8_t readRefWord8(int refIndex)
    {
    if (haskinoRefs[refIndex].ref != NULL)
        return *((uint8_t *) haskinoRefs[refIndex].ref);
    else
        {
        sendStringf("readRefWord8: Invalid ref index %d", refIndex);
        return false;
        }
    }

uint16_t readRefWord16(int refIndex)
    {
    if (haskinoRefs[refIndex].ref != NULL)
        return *((uint16_t *) haskinoRefs[refIndex].ref);
    else
        {
        sendStringf("readRefWord16: Invalid ref index %d", refIndex);
        return false;
        }
    }

uint32_t readRefWord32(int refIndex)
    {
    if (haskinoRefs[refIndex].ref != NULL)
        return *((uint32_t *) haskinoRefs[refIndex].ref);
    else
        {
        sendStringf("readRefWord32: Invalid ref index %d", refIndex);
        return false;
        }
    }

uint8_t *readRefList8(int refIndex)
    {
    if (haskinoRefs[refIndex].ref != NULL)
        return (uint8_t *) haskinoRefs[refIndex].ref;
    else
        {
        sendStringf("readRefList8: Invalid ref index %d", refIndex);
        return false;
        }
    }

static int typeToSize(int type)
    {
    switch(type)
        {
        case EXPR_BOOL:
        case EXPR_WORD16:
            return 2;
        case EXPR_WORD8:
            return 1;
        case EXPR_WORD32:
        default:
            return 4;
        }
    } 

void storeBoolRef(byte *expr, CONTEXT *context, byte refIndex)
    {
    bool bVal = evalBoolExpr(&expr, context);

    *((bool *) haskinoRefs[refIndex].ref) = bVal;
    }

void storeWord8Ref(byte *expr, CONTEXT *context, byte refIndex)
    {
    uint8_t w8Val = evalWord8Expr(&expr, context);

    *((uint8_t *) haskinoRefs[refIndex].ref) = w8Val;
    }

void storeWord16Ref(byte *expr, CONTEXT *context, byte refIndex)
    {
    uint16_t w16Val = evalWord16Expr(&expr, context);

    *((uint16_t *) haskinoRefs[refIndex].ref) = w16Val;
    }

void storeWord32Ref(byte *expr, CONTEXT *context, byte refIndex)
    {
    uint32_t w32Val = evalWord32Expr(&expr, context);

    *((uint32_t *) haskinoRefs[refIndex].ref) = w32Val;
    }

void storeList8Ref(byte *expr, CONTEXT *context, byte refIndex)
    {
    bool alloc;
    byte *lVal = evalList8Expr(&expr, context, &alloc);

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
        (((type != EXPR_LIST8) && (memory = malloc(spaceNeeded)) == NULL)))
        {
        sendReply(0, REF_RESP_NEW, NULL, context, bind);
        }
    else
        {
        if (type != EXPR_LIST8)
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
        newReply[0] = EXPR(type, EXPR_LIT);
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
        case EXPR_BOOL:
            readReply[0] = EXPR(EXPR_BOOL, EXPR_LIT);
            readReply[1] = *((bool *) haskinoRefs[refIndex].ref);
            sendReply(sizeof(bool)+1, REF_RESP_READ, readReply, context, bind);
            break;
        case EXPR_WORD8:
            readReply[0] = EXPR(EXPR_WORD8, EXPR_LIT);
            readReply[1] = *((uint8_t *) haskinoRefs[refIndex].ref);
            sendReply(sizeof(uint8_t)+1, REF_RESP_READ, readReply, context, bind);
            break;
        case EXPR_WORD16:
            readReply[0] = EXPR(EXPR_WORD16, EXPR_LIT);
            memcpy(&readReply[1], (byte *) haskinoRefs[refIndex].ref, sizeof(uint16_t));
            sendReply(sizeof(uint16_t)+1, REF_RESP_READ, readReply, context, bind);
            break;
        case EXPR_WORD32:
            readReply[0] = EXPR(EXPR_WORD32, EXPR_LIT);
            memcpy(&readReply[1], (byte *) haskinoRefs[refIndex].ref, sizeof(uint32_t));
            sendReply(sizeof(uint32_t)+1, REF_RESP_READ, readReply, context, bind);
            break;
        case EXPR_LIST8:
            lVal = (byte *) haskinoRefs[refIndex].ref;
            sendReply(lVal[1]+2, REF_RESP_READ, lVal, context, bind);
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
        case EXPR_LIST8:
            storeList8Ref(expr, context, refIndex);
            break;
        }
    return false;
    }
