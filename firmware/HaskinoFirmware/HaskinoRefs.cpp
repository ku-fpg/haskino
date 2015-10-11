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
    byte bind = msg[2];
    int count = 1;
    int spaceNeeded = typeToSize(type) * count;
    byte refIndex = msg[3];
    byte *expr = (byte *) &msg[4];
    void *memory;
    byte newReply[2];

    // ToDo: Handle overflow of ref numbers

    if ((refIndex >= MAX_REFS) ||
        ((memory = malloc(spaceNeeded)) == NULL))
        {
        sendReply(0, REF_RESP_NEW, NULL, local, bind);
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
                *((bool *) memory) = evalBoolExpr(&expr, local);
                break;
            case EXPR_WORD8:
                *((uint8_t *) memory) = evalWord8Expr(&expr, local);
                break;
            case EXPR_WORD16:
                *((uint16_t *) memory) = evalWord16Expr(&expr, local);
                break;
            case EXPR_WORD32:
                *((uint32_t *) memory) = evalWord32Expr(&expr, local);
                break;
            }
        newReply[0] = EXPR(EXPR_WORD8, EXPR_LIT);
        newReply[1] = refIndex;
        sendReply(sizeof(byte)+1, REF_RESP_NEW, newReply, local, bind);
        }
    return false;
    }

static bool handleReadRef(int type, int size, const byte *msg, byte *local)
    {
    byte bind = msg[2];
    byte *expr = (byte *) &msg[3];
    byte refIndex = evalWord8Expr(&expr, local);
    byte readReply[5];
    bool bVal;
    uint8_t w8Val;
    uint16_t w16Val;
    uint32_t w32Val;

    // ToDo:  Check for param errors
    switch (type)
        {
        case EXPR_BOOL:
            readReply[0] = EXPR(EXPR_BOOL, EXPR_LIT);
            readReply[1] = *((bool *) haskinoRefs[refIndex].ref);
            sendReply(sizeof(bool)+1, REF_RESP_READ, readReply, local, bind);
            break;
        case EXPR_WORD8:
            readReply[0] = EXPR(EXPR_WORD8, EXPR_LIT);
            readReply[1] = *((uint8_t *) haskinoRefs[refIndex].ref);
            sendReply(sizeof(uint8_t)+1, REF_RESP_READ, readReply, local, bind);
            break;
        case EXPR_WORD16:
            readReply[0] = EXPR(EXPR_WORD16, EXPR_LIT);
            memcpy(&readReply[1], (byte *) haskinoRefs[refIndex].ref, sizeof(uint16_t));
            sendReply(sizeof(uint16_t)+1, REF_RESP_READ, readReply, local, bind);
            break;
        case EXPR_WORD32:
            readReply[0] = EXPR(EXPR_WORD32, EXPR_LIT);
            memcpy(&readReply[1], (byte *) haskinoRefs[refIndex].ref, sizeof(uint32_t));
            sendReply(sizeof(uint32_t)+1, REF_RESP_READ, readReply, local, bind);
            break;
        }
    return false;
    }

static bool handleWriteRef(int type, int size, const byte *msg, byte *local)
    {
    byte *expr = (byte *) &msg[2];
    byte refIndex = evalWord8Expr(&expr, local);
    bool bVal;
    uint8_t w8Val;
    uint16_t w16Val;
    uint32_t w32Val;

    // ToDo:  Check for param errors

    switch (type)
        {
        case EXPR_BOOL:
            bVal = evalBoolExpr(&expr, local);
            *((bool *) haskinoRefs[refIndex].ref) = bVal;
            break;
        case EXPR_WORD8:
            w8Val = evalWord8Expr(&expr, local);
            *((uint8_t *) haskinoRefs[refIndex].ref) = w8Val;
            break;
        case EXPR_WORD16:
            w16Val = evalWord16Expr(&expr, local);
            *((uint16_t *) haskinoRefs[refIndex].ref) = w16Val;
            break;
        case EXPR_WORD32:
            w32Val = evalWord32Expr(&expr, local);
            *((uint32_t *) haskinoRefs[refIndex].ref) = w32Val;
            break;
        }
    return false;
    }
