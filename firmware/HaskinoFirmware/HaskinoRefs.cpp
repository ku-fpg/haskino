#include <Arduino.h>
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
static int nextFreeRefIndex = 0;

static bool handleNewRef(int type, int size, const byte *msg, byte *local);
static bool handleReadRef(int type, int size, const byte *msg, byte *local);
static bool handleWriteRef(int type, int size, const byte *msg);
static bool handleWriteEffectRef(int type, int size, const byte *msg);

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
            handleWriteRef(type, size, msg);
            break;
        case REF_CMD_WRITE_EFFECT:
            handleWriteEffectRef(type, size, msg);
            break;
        }
    return false;
    }

bool readRefBool(int refIndex)
    {
    if (refIndex < nextFreeRefIndex)
        return *((bool *) haskinoRefs[refIndex].ref);
    else
        {
        sendString("readRefBool: Invalid ref index");
        return false;
        }
    }

uint8_t readRefWord8(int refIndex)
    {
    if (refIndex < nextFreeRefIndex)
        return *((uint8_t *) haskinoRefs[refIndex].ref);
    else
        {
        sendString("readRefWord8: Invalid ref index");
        return false;
        }
    }

uint16_t readRefWord16(int refIndex)
    {
    if (refIndex < nextFreeRefIndex)
        return *((uint16_t *) haskinoRefs[refIndex].ref);
    else
        {
        sendString("readRefWord16: Invalid ref index");
        return false;
        }
    }

uint32_t readRefWord32(int refIndex)
    {
    if (refIndex < nextFreeRefIndex)
        return *((uint32_t *) haskinoRefs[refIndex].ref);
    else
        {
        sendString("readRefWord32: Invalid ref index");
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
    void *memory;
    byte refIndex;

    if ((nextFreeRefIndex >= MAX_REFS) ||
        ((memory = malloc(spaceNeeded)) == NULL))
        {
        // ToDo:  Verify sending empty message can indicate error
        sendReply(0, REF_RESP_NEW, NULL, local);
        }
    else
        {
        haskinoRefs[nextFreeRefIndex].ref = memory;
        haskinoRefs[nextFreeRefIndex].type = type;
        haskinoRefs[nextFreeRefIndex].len = count;
        refIndex = nextFreeRefIndex;
        sendReply(sizeof(byte), REF_RESP_NEW, (byte *) &refIndex, local);
        nextFreeRefIndex++;
        }
    return false;
    }

// ToDo:  Is this needed?  Probably not, perhaps a debug mechanism.
static bool handleReadRef(int type, int size, const byte *msg, byte *local)
    {
    byte refIndex = msg[1];
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

static bool handleWriteRef(int type, int size, const byte *msg)
    {
    byte refIndex = msg[1];
    byte *expr = (byte *) &msg[2];
    bool bVal;
    uint8_t w8Val;
    uint16_t w16Val;
    uint32_t w32Val;

    // ToDo:  Check for param errors

    switch (type)
        {
        case EXPR_BOOL:
            bVal = evalBoolExpr(&expr);
            *((bool *) haskinoRefs[refIndex].ref) = bVal;
            break;
        case EXPR_WORD8:
            w8Val = evalWord8Expr(&expr);
            *((uint8_t *) haskinoRefs[refIndex].ref) = w8Val;
            break;
        case EXPR_WORD16:
            w16Val = evalWord16Expr(&expr);
            *((uint16_t *) haskinoRefs[refIndex].ref) = w16Val;
            break;
        case EXPR_WORD32:
            w32Val = evalWord32Expr(&expr);
            *((uint32_t *) haskinoRefs[refIndex].ref) = w32Val;
            break;
        }
    return false;
    }

static bool handleWriteEffectRef(int type, int size, const byte *msg)
    {
    byte refIndex = msg[1];
    byte *expr = (byte *) &msg[2];
    bool bVal;
    uint8_t w8Val;
    uint16_t w16Val;
    uint32_t w32Val;

    // ToDo:  Check for param errors

    switch (type)
        {
        case EXPR_BOOL:
            bVal = evalBoolExpr(&expr);
            *((bool *) haskinoRefs[refIndex].ref) = bVal;
            break;
        case EXPR_WORD8:
            w8Val = evalWord8Expr(&expr);
            *((uint8_t *) haskinoRefs[refIndex].ref) = w8Val;
            break;
        case EXPR_WORD16:
            w16Val = evalWord16Expr(&expr);
            *((uint16_t *) haskinoRefs[refIndex].ref) = w16Val;
            break;
        case EXPR_WORD32:
            w32Val = evalWord32Expr(&expr);
            *((uint32_t *) haskinoRefs[refIndex].ref) = w32Val;
            break;
        }
    return false;
    }
