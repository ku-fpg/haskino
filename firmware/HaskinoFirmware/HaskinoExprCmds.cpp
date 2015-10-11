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
            reply[0] = EXPR(EXPR_BOOL,EXPR_LIT);
            boolResult = evalBoolExpr(&expr, local);
            memcpy(&reply[1], &boolResult, sizeof(boolResult));
            sendReply(1+sizeof(bool), EXP_RESP_EVAL, (const byte *) &reply, NULL, 0);
            break;
        case EXPR_WORD8:
            reply[0] = EXPR(EXPR_WORD8,EXPR_LIT);
            word8Result = evalWord8Expr(&expr, local);
            memcpy(&reply[1], &word8Result, sizeof(word8Result));
            sendReply(1+sizeof(uint8_t), EXP_RESP_EVAL, (const byte *) &reply, NULL, 0);
            break;
        case EXPR_WORD16:
            reply[0] = EXPR(EXPR_WORD16,EXPR_LIT);
            word16Result = evalWord16Expr(&expr, local);
            memcpy(&reply[1], &word16Result, sizeof(word16Result));
            sendReply(1+sizeof(uint16_t), EXP_RESP_EVAL, (const byte *) &reply, NULL, 0);
            break;
        case EXPR_WORD32:
            reply[0] = EXPR(EXPR_WORD32,EXPR_LIT);
            word32Result = evalWord32Expr(&expr, local);
            memcpy(&reply[1], &word16Result, sizeof(word16Result));
            sendReply(1+sizeof(uint32_t), EXP_RESP_EVAL, (const byte *) &reply, NULL, 0);
            break;
        }
        
 
    return false;
    }
