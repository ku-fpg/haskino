#include <Arduino.h>
#include "HaskinoConfig.h"
#include "HaskinoComm.h"
#include "HaskinoExpr.h"
#include "HaskinoRefs.h"

bool evalBoolExpr(byte **ppExpr); 
uint8_t evalWord8Expr(byte **ppExpr); 
uint16_t evalWord16Expr(byte **ppExpr); 
uint32_t evalWord32Expr(byte **ppExpr);

bool evalBoolExprOrBind(byte **ppExpr, byte *local)
    {
    byte *pExpr = *ppExpr;
    byte exprOp = *pExpr & EXPR_OP_MASK;

    if (exprOp == EXPR_BIND)
        {
        if (local == NULL)
            {
            return false;
            }
        else
            {
            return evalBoolExpr(&local);
            }
        }
    else
        {
        return evalBoolExpr(ppExpr);
        }
    }

bool evalBoolExpr(byte **ppExpr) 
    {
    byte *pExpr = *ppExpr;
    byte exprOp = *pExpr & EXPR_OP_MASK;
    bool val = false;
    int refNum;
    uint8_t e8_1,e8_2;
    uint16_t e16_1,e16_2;
    uint32_t e32_1,e32_2;
    byte exprType;

    switch (exprOp)
        {
        case EXPR_LIT:
            val = pExpr[1] == 0 ? false : true;
            *ppExpr += 2; // Use Cmd and Value bytes
            break;
        case EXPR_REF:
            refNum = pExpr[1];
            val = readRefBool(refNum);
            *ppExpr += 2; // Use Cmd and Ref bytes
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
            exprType = *pExpr >> EXPR_TYPE_SHFT;
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
                default:
                    sendStringf("Unknown ExType %d", exprType);
                }
            break;
        default:
            sendStringf("Unknown ExOp %d", exprOp);
        }
        return val;
    }

uint8_t evalWord8ExprOrBind(byte **ppExpr, byte *local)
    {
    byte *pExpr = *ppExpr;
    byte exprOp = *pExpr & EXPR_OP_MASK;

    if (exprOp == EXPR_BIND)
        {
        if (local == NULL)
            {
            return 0;
            }
        else
            {
            return evalWord8Expr(&local);
            }
        }
    else
        {
        return evalWord8Expr(ppExpr);
        }
    }

uint8_t evalWord8Expr(byte **ppExpr) 
    {
    byte *pExpr = *ppExpr;
    byte exprOp = *pExpr & EXPR_OP_MASK;
    uint8_t val = 0;
    uint8_t e1,e2;
    bool conditional;
    uint16_t thenSize, elseSize;
    int refNum;

    switch (exprOp)
        {
        case EXPR_LIT:
            val = pExpr[1];
            *ppExpr += 1 + sizeof(uint8_t); // Use Cmd and Value bytes
            break;
        case EXPR_REF:
            refNum = pExpr[1];
            val = readRefWord8(refNum);
             *ppExpr += 2; // Use Cmd and Ref bytes
            break;
        case EXPR_NEG:
        case EXPR_SIGN:
        case EXPR_COMP:
        case EXPR_BIT:
            *ppExpr += 1; // Use command byte
            e1 = evalWord8Expr(ppExpr);
            switch (exprOp)
                {
                case EXPR_NEG:
                    val = -e1;
                    break;
                case EXPR_SIGN:
                    val = e1 == 0 ? 0 : 1;
                    break;
                case EXPR_COMP:
                    val = ~e1;
                    break;
                case EXPR_BIT:
                    val = bit(e1);
                    break;
                }
            break;
        case EXPR_AND:
        case EXPR_OR:
        case EXPR_XOR:
        case EXPR_ADD:
        case EXPR_SUB:
        case EXPR_MULT:
        case EXPR_DIV:
        case EXPR_REM:
            *ppExpr += 1; // Use command byte
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
                case EXPR_SETB:
                    val = bitSet(e1, e2);
                    break;
                case EXPR_CLRB:
                    val = bitClear(e1, e2);
                    break;
                }
            break;
        case EXPR_IF:
            memcpy((byte *) &thenSize, &pExpr[1], sizeof(uint16_t));
            memcpy((byte *) &elseSize, &pExpr[3], sizeof(uint16_t));
            *ppExpr += 1 + 2*sizeof(uint16_t); // Use Cmd and Value bytes
            conditional = evalBoolExpr(ppExpr);
            if (conditional)
                {
                val = evalWord8Expr(ppExpr);
                *ppExpr += elseSize;
                }
            else
                {
                *ppExpr += thenSize;
                val = evalWord8Expr(ppExpr);
                }
            break;
        default:
            sendStringf("Unknown ExOp %d", exprOp);
        }
        return val;
    }

uint16_t evalWord16ExprOrBind(byte **ppExpr, byte *local)
    {
    byte *pExpr = *ppExpr;
    byte exprOp = *pExpr & EXPR_OP_MASK;

    if (exprOp == EXPR_BIND)
        {
        if (local == NULL)
            {
            return 0;
            }
        else
            {
            return evalWord16Expr(&local);
            }
        }
    else
        {
        return evalWord16Expr(ppExpr);
        }
    }

uint16_t evalWord16Expr(byte **ppExpr) 
    {
    byte *pExpr = *ppExpr;
    byte exprOp = *pExpr & EXPR_OP_MASK;
    uint16_t val = 0;
    uint16_t e1,e2;
    uint8_t e8_1;
    bool conditional;
    uint16_t thenSize, elseSize;
    int refNum;

    switch (exprOp)
        {
        case EXPR_LIT:
            memcpy((byte *) &val, &pExpr[1], sizeof(uint16_t));
            *ppExpr += 1 + sizeof(uint16_t); // Use Cmd and Value bytes
            break;
        case EXPR_REF:
            refNum = pExpr[1];
            val = readRefWord16(refNum);
            *ppExpr += 2; // Use Cmd and Ref bytes
            break;
        case EXPR_NEG:
        case EXPR_SIGN:
        case EXPR_COMP:
            *ppExpr += 1; // Use command byte
            e1 = evalWord16Expr(ppExpr);
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
            *ppExpr += 1; // Use command byte
            e1 = evalWord16Expr(ppExpr);
            e2 = evalWord16Expr(ppExpr);
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
                }
            break;
        case EXPR_SHFL:
        case EXPR_SHFR:
            *ppExpr += 1; // Use command byte
            e1 = evalWord16Expr(ppExpr);
            e8_1 = evalWord8Expr(ppExpr);
            switch(exprOp)
                {
                case EXPR_SHFL:
                    val = e1 << e8_1;
                    break;
                case EXPR_SHFR:
                    val = e1 >> e8_1;
                    break;
                case EXPR_SETB:
                    val = bitSet(e1, e8_1);
                    break;
                case EXPR_CLRB:
                    val = bitClear(e1, e8_1);
                    break;
                }
            break;
        case EXPR_IF:
            memcpy((byte *) &thenSize, &pExpr[1], sizeof(uint16_t));
            memcpy((byte *) &elseSize, &pExpr[3], sizeof(uint16_t));
            *ppExpr += 1 + 2*sizeof(uint16_t); // Use Cmd and Value bytes
            conditional = evalBoolExpr(ppExpr);
            if (conditional)
                {
                val = evalWord16Expr(ppExpr);
                *ppExpr += elseSize;
                }
            else
                {
                *ppExpr += thenSize;
                val = evalWord16Expr(ppExpr);
                }
            break;
        case EXPR_BIT:
            *ppExpr += 1; // Use command byte
            e1 = evalWord8Expr(ppExpr);
            val = bit(e1);
            break;
        default:
            sendStringf("Unknown ExOp %d", exprOp);
        }
        return val;
    }

uint32_t evalWord32ExprOrBind(byte **ppExpr, byte *local)
    {
    byte *pExpr = *ppExpr;
    byte exprOp = *pExpr & EXPR_OP_MASK;

    if (exprOp == EXPR_BIND)
        {
        if (local == NULL)
            {
            return 0;
            }
        else
            {
            return evalWord32Expr(&local);
            }
        }
    else
        {
        return evalWord32Expr(ppExpr);
        }
    }

uint32_t evalWord32Expr(byte **ppExpr) 
    {
    byte *pExpr = *ppExpr;
    byte exprOp = *pExpr & EXPR_OP_MASK;
    uint32_t val = 0;
    uint32_t e1,e2;
    uint8_t e8_1;
    bool conditional;
    uint16_t thenSize, elseSize;
    int refNum;

    switch (exprOp)
        {
        case EXPR_LIT:
            memcpy((byte *) &val, &pExpr[1], sizeof(uint32_t));
            *ppExpr += 1 + sizeof(uint32_t); // Use Cmd and Value bytes
            break;
        case EXPR_REF:
            refNum = pExpr[1];
            val = readRefWord32(refNum);
            *ppExpr += 2; // Use Cmd and Ref bytes
            break;
        case EXPR_NEG:
        case EXPR_SIGN:
        case EXPR_COMP:
            *ppExpr += 1; // Use command byte
            e1 = evalWord32Expr(ppExpr);
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
            *ppExpr += 1; // Use command byte
            e1 = evalWord32Expr(ppExpr);
            e2 = evalWord32Expr(ppExpr);
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
                }
            break;
        case EXPR_SHFR:
        case EXPR_SHFL:
            *ppExpr += 1; // Use command byte
            e1 = evalWord32Expr(ppExpr);
            e8_1 = evalWord8Expr(ppExpr);
            switch(exprOp)
                {
                case EXPR_SHFL:
                    val = e1 << e8_1;
                    break;
                case EXPR_SHFR:
                    val = e1 >> e8_1;
                    break;
                case EXPR_SETB:
                    val = bitSet(e1, e8_1);
                    break;
                case EXPR_CLRB:
                    val = bitClear(e1, e8_1);
                    break;
                }
            break;
        case EXPR_IF:
            memcpy((byte *) &thenSize, &pExpr[1], sizeof(uint16_t));
            memcpy((byte *) &elseSize, &pExpr[3], sizeof(uint16_t));
            *ppExpr += 1 + 2*sizeof(uint16_t); // Use Cmd and Value bytes
            conditional = evalBoolExpr(ppExpr);
            if (conditional)
                {
                val = evalWord32Expr(ppExpr);
                *ppExpr += elseSize;
                }
            else
                {
                *ppExpr += thenSize;
                val = evalWord32Expr(ppExpr);
                }
            break;
        case EXPR_BIT:
            *ppExpr += 1; // Use command byte
            e1 = evalWord8Expr(ppExpr);
            val = bit(e1);
            break;
        default:
            sendStringf("Unknown ExOp %d", exprOp);
        }
        return val;
    }
