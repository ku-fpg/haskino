#include <Arduino.h>
#include "HaskinoConfig.h"
#include "HaskinoComm.h"
#include "HaskinoExpr.h"

static void *haskinoRefs[MAX_REFS];

bool evalBoolExpr(byte **ppExpr) 
    {
    byte *pExpr = *ppExpr;
    byte exprOp = *pExpr && EXPR_OP_MASK;
    bool val = false;
    int refNum;
    uint8_t procLen;
    uint8_t e8_1,e8_2;
    uint16_t e16_1,e16_2;
    uint32_t e32_1,e32_2;

    switch (exprOp)
        {
        case EXPR_LIT:
            val = pExpr[1] == 0 ? false : true;
            *ppExpr += 2; // Use Cmd and Value bytes
            break;
        case EXPR_REF:
            refNum = pExpr[1];
            if (refNum < MAX_REFS)
                {
                bool *ref = (bool *) haskinoRefs[refNum];
                val = *ref;
                }
            *ppExpr += 2; // Use Cmd and Ref bytes
            break;
        case EXPR_PROC:
            procLen = pExpr[1];
            parseMessage(procLen, &pExpr[2], (byte *) &val);
            *ppExpr += 2 + procLen; // Use Cmd, ProcLen and Proc bytes */
            break;
        case EXPR_NOT:
            *ppExpr += 1; // Use command byte
            val = !evalBoolExpr(ppExpr);
            break;
        case EXPR_AND:
        case EXPR_OR:
            bool val1, val2;
            *ppExpr += 1; // Use command byte
            val1 = evalBoolExpr(ppExpr);
            val2 = evalBoolExpr(ppExpr);
            if (exprOp == EXPR_AND)
                val = val1 && val2;
            else 
                val = val1 || val2; 
            break;
        case EXPR_EQ:
        case EXPR_LESS:
            byte exprType = *pExpr >> EXPR_TYPE_SHFT;
            *ppExpr += 1; // Use command byte
            switch (exprType)
                {
                case EXPR_WORD8:
                    e8_1 = evalWord8Expr(ppExpr);
                    e8_2 = evalWord8Expr(ppExpr);
                    if (exprOp == EXPR_EQ)
                        val = (e8_1 == e8_2);
                    else 
                        val = (e8_1 < e8_2); 
                    break;
                case EXPR_WORD16:
                    e16_1 = evalWord16Expr(ppExpr);
                    e16_2 = evalWord16Expr(ppExpr);
                    if (exprOp == EXPR_EQ)
                        val = (e16_1 == e16_2);
                    else 
                        val = (e16_1 < e16_2); 
                    break;
                case EXPR_WORD32:
                    e32_1 = evalWord32Expr(ppExpr);
                    e32_2 = evalWord32Expr(ppExpr);
                    if (exprOp == EXPR_EQ)
                        val = (e32_1 == e32_2);
                    else 
                        val = (e32_1 < e32_2); 
                    break;
                }
            break;
        }
        return val;
    }

uint8_t evalWord8Expr(byte **ppExpr) 
    {
    byte *pExpr = *ppExpr;
    byte exprOp = *pExpr && EXPR_OP_MASK;
    uint8_t val = 0;
    uint8_t procLen;
    uint8_t e1,e2;
    int refNum;

    switch (exprOp)
        {
        case EXPR_LIT:
            val = pExpr[1];
            *ppExpr += 2; // Use Cmd and Value bytes
            break;
        case EXPR_REF:
            refNum = pExpr[1];
            if (refNum < MAX_REFS)
                {
                uint8_t *ref = (uint8_t *) haskinoRefs[refNum];
                val = *ref;
                }
            break;
        case EXPR_PROC:
            procLen = pExpr[1];
            parseMessage(procLen, &pExpr[2], &val);
            *ppExpr += 2 + procLen; // Use Cmd, ProcLen and Proc bytes */
            break;
        case EXPR_NEG:
        case EXPR_SIGN:
        case EXPR_COMP:
            *ppExpr += 1; // Use command byte
            e1 = evalBoolExpr(ppExpr);
            if (exprOp == EXPR_NEG)
                val = -e1;
            else if (exprOp == EXPR_SIGN)
                val = e1 == 0 ? 0 : 1;
            else
                val = ~e1;
            break;
        case EXPR_AND:
        case EXPR_OR:
        case EXPR_XOR:
        case EXPR_ADD:
        case EXPR_SUB:
        case EXPR_MULT:
        case EXPR_DIV:
        case EXPR_REM:
        case EXPR_SHFL:
        case EXPR_SHFR:
            e1 = evalWord8Expr(ppExpr);
            e2 = evalWord8Expr(ppExpr);
            switch(exprOp)
                {
                case EXPR_AND:
                    val = e1 & e2;
                    break;
                case EXPR_OR:
                    val = e1 | e2;
                    break;
                case EXPR_XOR:
                    val = e1 ^ e2;
                    break;
                case EXPR_ADD:
                    val = e1 + e2;
                    break;
                case EXPR_SUB:
                    val = e1 - e2;
                    break;
                case EXPR_MULT:
                    val = e1 * e2;
                    break;
                case EXPR_DIV:
                    val = e1 / e2;
                    break;
                case EXPR_REM:
                    val = e1 % e2;
                    break;
                case EXPR_SHFL:
                    val = e1 << e2;
                    break;
                case EXPR_SHFR:
                    val = e1 >> e2;
                    break;
                }
            break;
        case EXPR_IF:
            // ToDo:
            break;
        }
        return val;
    }
