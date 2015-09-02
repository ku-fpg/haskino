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
static bool handleModifyRef(int type, int size, const byte *msg);

bool parseRefMessage(int size, const byte *msg, byte *local)
    {
    byte type = msg[0] && REF_CMD_TYPE_MASK;
    byte cmd = msg[0] >> REF_CMD_SUBCMD_SHIFT;

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
        case REF_CMD_MOD:
            handleModifyRef(type, size, msg);
            break;
        }
    return false;
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
            return 4;
        }
    } 

static bool handleNewRef(int type, int size, const byte *msg, byte *local)
    {
    int count = 1;
    int spaceNeeded = typeToSize(type) * count;
    void *memory;

    if ((nextFreeRefIndex >= MAX_REFS) ||
        ((memory = malloc(spaceNeeded)) == NULL))
        {
        // ToDo:  How to report error - 
        }
    else
        {
        haskinoRefs[nextFreeRefIndex].ref = memory;
        haskinoRefs[nextFreeRefIndex].type = type;
        haskinoRefs[nextFreeRefIndex].len = count;
        }
    return false;
    }

static bool handleReadRef(int type, int size, const byte *msg, byte *local)
    {
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
    return false;
    }

static bool handleModifyRef(int type, int size, const byte *msg)
    {
    return false;
    }
