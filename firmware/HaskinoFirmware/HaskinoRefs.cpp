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

static bool handleNewRef(int type, int size, const byte *msg, byte *local);
static bool handleReadRef(int type, int size, const byte *msg, byte *local);
static bool handleWriteRef(int type, int size, const byte *msg, byte *local);
/* static bool handleWriteEffectRef(int type, int size, const byte *msg); */

bool parseRefMessage(int size, const byte *msg, byte *local)
    {
    byte cmd = msg[0];
    byte type = msg[1];

    switch (cmd) 
        {
        case REF_CMD_NEW:
            handleNewRef(type, size, msg, local);
            break;
        case REF_CMD_READ:
            handleReadRef(type, size, msg, local);
            break;
        case REF_CMD_WRITE:
            handleWriteRef(type, size, msg, local);
            break;
/*        case REF_CMD_WRITE_EFFECT:
            handleWriteEffectRef(type, size, msg);
            break; */
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

static bool handleNewRef(int type, int size, const byte *msg, byte *local)
    {
    int count = 1;
    int spaceNeeded = typeToSize(type) * count;
    byte refIndex = msg[2];
    byte *expr = (byte *) &msg[3];
    void *memory;

    if ((refIndex >= MAX_REFS) ||
        ((memory = malloc(spaceNeeded)) == NULL))
        {
        sendReply(0, REF_RESP_NEW, NULL, local);
        }
    else
        {
        if (haskinoRefs[refIndex].ref != NULL)
            free(haskinoRefs[refIndex].ref);
        haskinoRefs[refIndex].ref = memory;
        haskinoRefs[refIndex].type = type;
        haskinoRefs[refIndex].len = count;
        switch (type)
            {
            case EXPR_BOOL:
                *((bool *) memory) = evalBoolExprOrBind(&expr, local);
                break;
            case EXPR_WORD8:
                *((uint8_t *) memory) = evalWord8ExprOrBind(&expr, local);
                break;
            case EXPR_WORD16:
                *((uint16_t *) memory) = evalWord16ExprOrBind(&expr, local);
                break;
            case EXPR_WORD32:
                *((uint32_t *) memory) = evalWord32ExprOrBind(&expr, local);
                break;
            }
        sendReply(sizeof(byte), REF_RESP_NEW, (byte *) &refIndex, local);
        }
    return false;
    }

// ToDo:  Is this needed?  Probably not, perhaps a debug mechanism.
static bool handleReadRef(int type, int size, const byte *msg, byte *local)
    {
    byte refIndex = msg[2];
    bool bVal;
    uint8_t w8Val;
    uint16_t w16Val;
    uint32_t w32Val;

    // ToDo:  Check for param errors

    switch (type)
        {
        case EXPR_BOOL:
            bVal = *((bool *) haskinoRefs[refIndex].ref);
            sendReply(sizeof(bool), REF_RESP_READ, (byte *) &bVal, local);
            break;
        case EXPR_WORD8:
            w8Val = *((uint8_t *) haskinoRefs[refIndex].ref);
            sendReply(sizeof(uint8_t), REF_RESP_READ, (byte *) &w8Val, local);
            break;
        case EXPR_WORD16:
            w16Val = *((uint16_t *) haskinoRefs[refIndex].ref);
            sendReply(sizeof(uint16_t), REF_RESP_READ, (byte *) &w16Val, local);
            break;
        case EXPR_WORD32:
            w32Val = *((uint32_t *) haskinoRefs[refIndex].ref);
            sendReply(sizeof(uint32_t), REF_RESP_READ, (byte *) &w32Val, local);
            break;
        }
    return false;
    }

static bool handleWriteRef(int type, int size, const byte *msg, byte *local)
    {
    byte refIndex = msg[2];
    byte *expr = (byte *) &msg[3];
    bool bVal;
    uint8_t w8Val;
    uint16_t w16Val;
    uint32_t w32Val;

    // ToDo:  Check for param errors

    switch (type)
        {
        case EXPR_BOOL:
            bVal = evalBoolExprOrBind(&expr, local);
            *((bool *) haskinoRefs[refIndex].ref) = bVal;
            break;
        case EXPR_WORD8:
            w8Val = evalWord8ExprOrBind(&expr, local);
            *((uint8_t *) haskinoRefs[refIndex].ref) = w8Val;
            break;
        case EXPR_WORD16:
            w16Val = evalWord16ExprOrBind(&expr, local);
            *((uint16_t *) haskinoRefs[refIndex].ref) = w16Val;
            break;
        case EXPR_WORD32:
            w32Val = evalWord32ExprOrBind(&expr, local);
            *((uint32_t *) haskinoRefs[refIndex].ref) = w32Val;
            break;
        }
    return false;
    }

/*static bool handleWriteEffectRef(int type, int size, const byte *msg)
    {
    byte refIndex = msg[2];
    byte *codeBlock = (byte *) &msg[3];
    int blockSize = size - 3;
    bool bVal;
    uint8_t w8Val;
    uint16_t w16Val;
    uint32_t w32Val;
    uint32_t val;

    // ToDo:  Check for param errors
    runCodeBlock(blockSize, codeBlock,(byte *) &val);

    switch (type)
        {
        case EXPR_BOOL:
            memcpy((byte *) &bVal, (const byte *) &val, sizeof(bVal));
            *((bool *) haskinoRefs[refIndex].ref) = bVal;
            break;
        case EXPR_WORD8:
            memcpy((byte *) &w8Val, (const byte *) &val, sizeof(w8Val));
            *((uint8_t *) haskinoRefs[refIndex].ref) = w8Val;
            break;
        case EXPR_WORD16:
            memcpy((byte *) &w16Val, (const byte *) &val, sizeof(w16Val));
            *((uint16_t *) haskinoRefs[refIndex].ref) = w16Val;
            break;
        case EXPR_WORD32:
            memcpy((byte *) &w32Val, (const byte *) &val, sizeof(w32Val));
            *((uint32_t *) haskinoRefs[refIndex].ref) = w32Val;
            break;
        }
    return false;
    } */
