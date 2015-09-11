#include <Arduino.h>
#include "HaskinoComm.h"
#include "HaskinoCommands.h"
#include "HaskinoConfig.h"
#include "HaskinoExpr.h"
#include "HaskinoExprCmds.h"

static bool handleEval(int type, int size, const byte *msg, byte *local);

bool parseExprMessage(int size, const byte *msg, byte *local)
    {
    byte cmd = msg[0];
    byte type = msg[1];

    switch (cmd) 
        {
        case EXP_CMD_EVAL:
            handleEval(type, size, msg, local);
            break;
        }
    return false;
    }

static bool handleEval(int type, int size, const byte *msg, byte *local)
    {
    byte *expr = (byte *) &msg[2];
    byte reply[5];
    bool boolResult;
    uint8_t word8Result;
    uint16_t word16Result;
    uint32_t word32Result;

    switch (type)
        {
        case EXPR_BOOL:
            reply[0] = EXPR_BOOL;
            boolResult = evalBoolExpr(&expr);
            memcpy(&reply[1], &boolResult, sizeof(boolResult));
            sendReply(1+sizeof(bool), EXP_RESP_EVAL, (const byte *) &reply, local);
            break;
        case EXPR_WORD8:
            reply[0] = EXPR_WORD8;
            word8Result = evalWord8Expr(&expr);
            memcpy(&reply[1], &word8Result, sizeof(word8Result));
            sendReply(1+sizeof(uint8_t), EXP_RESP_EVAL, (const byte *) &reply, local);
            break;
        case EXPR_WORD16:
            reply[0] = EXPR_WORD16;
            word16Result = evalWord16Expr(&expr);
            memcpy(&reply[1], &word16Result, sizeof(word16Result));
            sendReply(1+sizeof(uint16_t), EXP_RESP_EVAL, (const byte *) &reply, local);
            break;
        case EXPR_WORD32:
            reply[0] = EXPR_WORD32;
            word32Result = evalWord32Expr(&expr);
            memcpy(&reply[1], &word16Result, sizeof(word16Result));
            sendReply(1+sizeof(uint32_t), EXP_RESP_EVAL, (const byte *) &reply, local);
            break;
        }
        
 
    return false;
    }
