#include <Arduino.h>
#include "HaskinoConfig.h"
#include "HaskinoComm.h"
#include "HaskinoExpr.h"
#include "HaskinoRefs.h"

bool evalBoolExpr(byte **ppExpr, CONTEXT *context) 
    {
    byte *pExpr = *ppExpr;
    byte exprOp = *pExpr & EXPR_OP_MASK;
    bool val = false;
    int refNum;
    bool conditional;
    uint16_t thenSize, elseSize;
    bool eB_1,eB_2;
    uint8_t e8_1,e8_2,l1len,l2len;
    uint16_t e16_1,e16_2;
    uint32_t e32_1,e32_2;
    int8_t ei8_1,ei8_2;
    int16_t ei16_1,ei16_2;
    int32_t ei32_1,ei32_2;
    uint8_t *l1, *l2;
    bool alloc1, alloc2;
    byte exprType;
    byte bind;
    byte *bindPtr;

    switch (exprOp)
        {
        case EXPR_BIND:
            bind = pExpr[1];
            if (!context->bind)
                {
                val = false;
                }
            else
                {
                bindPtr = &context->bind[bind * BIND_SPACING];
                val = evalBoolExpr(&bindPtr, context);
                }
            *ppExpr += 2; // Use Cmd and Bind Index bytes
            break;
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
            val = !evalBoolExpr(ppExpr, context);
            break;
        case EXPR_AND:
        case EXPR_OR:
            bool val1, val2;
            *ppExpr += 1; // Use command byte
            val1 = evalBoolExpr(ppExpr, context);
            val2 = evalBoolExpr(ppExpr, context);
            if (exprOp == EXPR_AND)
                val = val1 && val2;
            else 
                val = val1 || val2; 
            break;
        case EXPR_IF:
            memcpy((byte *) &thenSize, &pExpr[1], sizeof(uint16_t));
            memcpy((byte *) &elseSize, &pExpr[3], sizeof(uint16_t));
            *ppExpr += 1 + 2*sizeof(uint16_t); // Use Cmd and Value bytes
            conditional = evalBoolExpr(ppExpr, context);
            if (conditional)
                {
                val = evalBoolExpr(ppExpr, context);
                *ppExpr += elseSize;
                }
            else
                {
                *ppExpr += thenSize;
                val = evalBoolExpr(ppExpr, context);
                }
            break;
        case EXPR_EQ:
        case EXPR_LESS:
            exprType = *pExpr >> EXPR_TYPE_SHFT;
            *ppExpr += 1; // Use command byte
            switch (exprType)
                {
                case EXPR_BOOL:
                    eB_1 = evalBoolExpr(ppExpr, context);
                    eB_2 = evalBoolExpr(ppExpr, context);
                    if (exprOp == EXPR_EQ)
                        val = (eB_1 == eB_2);
                    else 
                        val = (eB_1 < eB_2); 
                    break;
                case EXPR_WORD8:
                    e8_1 = evalWord8Expr(ppExpr, context);
                    e8_2 = evalWord8Expr(ppExpr, context);
                    if (exprOp == EXPR_EQ)
                        val = (e8_1 == e8_2);
                    else 
                        val = (e8_1 < e8_2); 
                    break;
                case EXPR_WORD16:
                    e16_1 = evalWord16Expr(ppExpr, context);
                    e16_2 = evalWord16Expr(ppExpr, context);
                    if (exprOp == EXPR_EQ)
                        val = (e16_1 == e16_2);
                    else 
                        val = (e16_1 < e16_2); 
                    break;
                case EXPR_WORD32:
                    e32_1 = evalWord32Expr(ppExpr, context);
                    e32_2 = evalWord32Expr(ppExpr, context);
                    if (exprOp == EXPR_EQ)
                        val = (e32_1 == e32_2);
                    else 
                        val = (e32_1 < e32_2); 
                    break;
                case EXPR_INT8:
                    ei8_1 = evalInt8Expr(ppExpr, context);
                    ei8_2 = evalInt8Expr(ppExpr, context);
                    if (exprOp == EXPR_EQ)
                        val = (ei8_1 == ei8_2);
                    else 
                        val = (ei8_1 < ei8_2); 
                    break;
                case EXPR_INT16:
                    ei16_1 = evalInt16Expr(ppExpr, context);
                    ei16_2 = evalInt16Expr(ppExpr, context);
                    if (exprOp == EXPR_EQ)
                        val = (ei16_1 == ei16_2);
                    else 
                        val = (ei16_1 < ei16_2); 
                    break;
                case EXPR_INT32:
                    ei32_1 = evalInt32Expr(ppExpr, context);
                    ei32_2 = evalInt32Expr(ppExpr, context);
                    if (exprOp == EXPR_EQ)
                        val = (ei32_1 == ei32_2);
                    else 
                        val = (ei32_1 < ei32_2); 
                    break;
                case EXPR_LIST8:
                    l1 = evalList8Expr(ppExpr, context, &alloc1);
                    l2 = evalList8Expr(ppExpr, context, &alloc2);
                    l1len = l1[1];
                    l2len = l2[1];
                    if (exprOp == EXPR_EQ)
                        {
                        if (l1len != l2len)
                            val = false;
                        else 
                            {
                            val = true;
                            for (int i=0;i<l1len;i++)
                                {
                                if (l1[2+i] != l2[2+i])
                                    {
                                    val = false;
                                    break;
                                    }
                                }
                            }
                        }
                    else
                        {
                        int i;
                        for (i=0;
                             i < l1len && i < l2len && l1[2+i] == l2[2+i];
                             i++);
                        if (i == l1len && i == l2len)
                            val = false;
                        else if (i == l1len)
                            val = true;
                        else if (i == l2len)
                            val = false;
                        else 
                            val = l1[2+i] < l2[2+i];
                        }
                    if (alloc1)
                        free(l1);
                    if (alloc2)
                        free(l2);
                    break;
                default:
                    sendStringf("eBE:T%d", exprType);
                }
            break;
        case EXPR_TSTB:
            exprType = *pExpr >> EXPR_TYPE_SHFT;
            *ppExpr += 1; // Use command byte
            switch (exprType)
                {
                case EXPR_WORD8:
                    e8_1 = evalWord8Expr(ppExpr, context);
                    e8_2 = evalWord8Expr(ppExpr, context);
                    if (e8_2 > 7)
                        val = false;
                    else
                        val = (e8_1 & (1 << e8_2)) != 0;
                    break;
                case EXPR_WORD16:
                    e16_1 = evalWord16Expr(ppExpr, context);
                    e8_2 = evalWord8Expr(ppExpr, context);
                    if (e8_2 > 15)
                        val = false;
                    else
                        val = (e16_1 & (1 << e8_2)) != 0;
                    break;
                case EXPR_WORD32:
                    e32_1 = evalWord32Expr(ppExpr, context);
                    e8_2 = evalWord8Expr(ppExpr, context);
                    if (e8_2 > 31)
                        val = false;
                    else
                        val = (e32_1 & (1 << e8_2)) != 0;
                    break;
                case EXPR_INT8:
                    ei8_1 = evalInt8Expr(ppExpr, context);
                    e8_2 = evalWord8Expr(ppExpr, context);
                    if (e8_2 > 7)
                        val = false;
                    else
                        val = (ei8_1 & (1 << e8_2)) != 0;
                    break;
                case EXPR_INT16:
                    ei16_1 = evalInt16Expr(ppExpr, context);
                    e8_2 = evalWord8Expr(ppExpr, context);
                    if (e8_2 > 15)
                        val = false;
                    else
                        val = (ei16_1 & (1 << e8_2)) != 0;
                    break;
                case EXPR_INT32:
                    ei32_1 = evalInt32Expr(ppExpr, context);
                    e8_2 = evalWord8Expr(ppExpr, context);
                    if (e8_2 > 31)
                        val = false;
                    else
                        val = (ei32_1 & (1 << e8_2)) != 0;
                    break;
                }
            break;
        default:
            sendStringf("eBE:O%d", exprOp);
        }
        return val;
    }

uint8_t evalWord8Expr(byte **ppExpr, CONTEXT *context) 
    {
    byte *pExpr = *ppExpr;
    byte exprOp = *pExpr & EXPR_OP_MASK;
    uint8_t val = 0;
    uint8_t e1,e2;
    bool conditional;
    uint16_t thenSize, elseSize;
    uint8_t *listMem;
    bool alloc;
    uint8_t index;
    int refNum;
    byte bind;
    byte *bindPtr;

    switch (exprOp)
        {
        case EXPR_BIND:
            bind = pExpr[1];
            if (!context->bind)
                {
                val = 0;
                }
            else
                {
                bindPtr = &context->bind[bind * BIND_SPACING];
                val = evalWord8Expr(&bindPtr, context);
                }
            *ppExpr += 2; // Use Cmd and Bind Index bytes
            break;
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
            *ppExpr += 1; // Use command byte
            e1 = evalWord8Expr(ppExpr, context);
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
        case EXPR_SHFL:
        case EXPR_SHFR:
        case EXPR_SETB:
        case EXPR_CLRB:
            *ppExpr += 1; // Use command byte
            e1 = evalWord8Expr(ppExpr, context);
            e2 = evalWord8Expr(ppExpr, context);
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
                    if (e2 > 8) e2 = 8;
                    val = e1 << e2;
                    break;
                case EXPR_SHFR:
                    if (e2 > 8) e2 = 8;
                    val = e1 >> e2;
                    break;
                case EXPR_SETB:
                    if (e2 > 7)
                        val = e1;
                    else
                        val = bitSet(e1, e2);
                    break;
                case EXPR_CLRB:
                    if (e2 > 7)
                        val = e1;
                    else
                        val = bitClear(e1, e2);
                    break;
                }
            break;
        case EXPR_FINT:
            *ppExpr += 1; // Use command byte
            val = evalWord32Expr(ppExpr, context);
            break;
        case EXPR_IF:
            memcpy((byte *) &thenSize, &pExpr[1], sizeof(uint16_t));
            memcpy((byte *) &elseSize, &pExpr[3], sizeof(uint16_t));
            *ppExpr += 1 + 2*sizeof(uint16_t); // Use Cmd and Value bytes
            conditional = evalBoolExpr(ppExpr, context);
            if (conditional)
                {
                val = evalWord8Expr(ppExpr, context);
                *ppExpr += elseSize;
                }
            else
                {
                *ppExpr += thenSize;
                val = evalWord8Expr(ppExpr, context);
                }
            break;
        case EXPR_LEN:
            *ppExpr += 1; // Use command byte
            listMem = evalList8Expr(ppExpr, context, &alloc);
            val = listMem[1];
            if (alloc)
                free(listMem);
            break;
        case EXPR_ELEM:
            *ppExpr += 1; // Use command byte
            listMem = evalList8Expr(ppExpr, context, &alloc);
            index = evalWord8Expr(ppExpr, context);
            if (index < listMem[1])
                val = listMem[2+index];
            else // ToDo: handle out of bound index
                val = 0;
            if (alloc)
                free(listMem);
            break;
        default:
            sendStringf("eW8E:O%d", exprOp);
        }
        return val;
    }

int8_t evalInt8Expr(byte **ppExpr, CONTEXT *context) 
    {
    byte *pExpr = *ppExpr;
    byte exprOp = *pExpr & EXPR_OP_MASK;
    int8_t val = 0;
    int8_t e1,e2,e3;
    uint8_t e8_1;
    bool conditional;
    uint16_t thenSize, elseSize;
    int refNum;
    byte bind;
    byte *bindPtr;

    switch (exprOp)
        {
        case EXPR_BIND:
            bind = pExpr[1];
            if (!context->bind)
                {
                val = 0;
                }
            else
                {
                bindPtr = &context->bind[bind * BIND_SPACING];
                val = evalInt8Expr(&bindPtr, context);
                }
            *ppExpr += 2; // Use Cmd and Bind Index bytes
            break;
        case EXPR_LIT:
            val = pExpr[1];
            *ppExpr += 1 + sizeof(int8_t); // Use Cmd and Value bytes
            break;
        case EXPR_REF:
            refNum = pExpr[1];
            val = readRefInt8(refNum);
             *ppExpr += 2; // Use Cmd and Ref bytes
            break;
        case EXPR_NEG:
        case EXPR_SIGN:
        case EXPR_COMP:
            *ppExpr += 1; // Use command byte
            e1 = evalInt8Expr(ppExpr, context);
            switch (exprOp)
                {
                case EXPR_NEG:
                    val = -e1;
                    break;
                case EXPR_SIGN:
                    if (e1 < 0)
                        val = -1;
                    else if (e1 == 0)
                        val = 0;
                    else
                        val = 1;
                    break;
                case EXPR_COMP:
                    val = ~e1;
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
            e1 = evalInt8Expr(ppExpr, context);
            e2 = evalInt8Expr(ppExpr, context);
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
                    e3 = e1 % e2;
                    if ((e3 != 0) && ((e3 < 0) != (e2 < 0)))
                        val = e1 / e2 - 1;
                    else
                        val = e1 / e2;
                    break;
                case EXPR_REM:
                    val = e1 % e2;
                    break;
                }
            break;
        case EXPR_SHFL:
        case EXPR_SHFR:
        case EXPR_SETB:
        case EXPR_CLRB:
            *ppExpr += 1; // Use command byte
            e1 = evalInt8Expr(ppExpr, context);
            e8_1 = evalWord8Expr(ppExpr, context);
            switch(exprOp)
                {
                case EXPR_SHFL:
                    if (e8_1 > 8) e8_1 = 8;
                    val = e1 << e8_1;
                    break;
                case EXPR_SHFR:
                    if (e8_1 > 8) e8_1 = 8;
                    val = e1 >> e8_1;
                    break;
                case EXPR_SETB:
                    if (e8_1 > 7)
                        val = e1;
                    else
                        val = bitSet(e1, e8_1);
                    break;
                case EXPR_CLRB:
                    if (e8_1 > 7)
                        val = e1;
                    else
                        val = bitClear(e1, e8_1);
                    break;
                }
            break;
        case EXPR_FINT:
            *ppExpr += 1; // Use command byte
            val = evalWord32Expr(ppExpr, context);
            break;
        case EXPR_IF:
            memcpy((byte *) &thenSize, &pExpr[1], sizeof(uint16_t));
            memcpy((byte *) &elseSize, &pExpr[3], sizeof(uint16_t));
            *ppExpr += 1 + 2*sizeof(uint16_t); // Use Cmd and Value bytes
            conditional = evalBoolExpr(ppExpr, context);
            if (conditional)
                {
                val = evalInt8Expr(ppExpr, context);
                *ppExpr += elseSize;
                }
            else
                {
                *ppExpr += thenSize;
                val = evalInt8Expr(ppExpr, context);
                }
            break;
        default:
            sendStringf("eI8E:O%d", exprOp);
        }
        return val;
    }

uint16_t evalWord16Expr(byte **ppExpr, CONTEXT *context) 
    {
    byte *pExpr = *ppExpr;
    byte exprOp = *pExpr & EXPR_OP_MASK;
    uint16_t val = 0;
    uint16_t e1,e2;
    uint8_t e8_1;
    bool conditional;
    uint16_t thenSize, elseSize;
    int refNum;
    byte bind;
    byte *bindPtr;

    switch (exprOp)
        {
        case EXPR_BIND:
            bind = pExpr[1];
            if (!context->bind)
                {
                val = 0;
                }
            else
                {
                bindPtr = &context->bind[bind * BIND_SPACING];
                val = evalWord16Expr(&bindPtr, context);
                }
            *ppExpr += 2; // Use Cmd and Bind Index bytes
            break;
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
            e1 = evalWord16Expr(ppExpr, context);
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
            e1 = evalWord16Expr(ppExpr, context);
            e2 = evalWord16Expr(ppExpr, context);
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
        case EXPR_SETB:
        case EXPR_CLRB:
            *ppExpr += 1; // Use command byte
            e1 = evalWord16Expr(ppExpr, context);
            e8_1 = evalWord8Expr(ppExpr, context);
            switch(exprOp)
                {
                case EXPR_SHFL:
                    if (e8_1 > 16) e8_1 = 16;
                    val = e1 << e8_1;
                    break;
                case EXPR_SHFR:
                    if (e8_1 > 16) e8_1 = 16;
                    val = e1 >> e8_1;
                    break;
                case EXPR_SETB:
                    if (e8_1 > 15)
                        val = e1;
                    else
                        val = bitSet(e1, e8_1);
                    break;
                case EXPR_CLRB:
                    if (e8_1 > 15)
                        val = e1;
                    else
                        val = bitClear(e1, e8_1);
                    break;
                }
            break;
        case EXPR_FINT:
            *ppExpr += 1; // Use command byte
            val = evalWord32Expr(ppExpr, context);
            break;
        case EXPR_IF:
            memcpy((byte *) &thenSize, &pExpr[1], sizeof(uint16_t));
            memcpy((byte *) &elseSize, &pExpr[3], sizeof(uint16_t));
            *ppExpr += 1 + 2*sizeof(uint16_t); // Use Cmd and Value bytes
            conditional = evalBoolExpr(ppExpr, context);
            if (conditional)
                {
                val = evalWord16Expr(ppExpr, context);
                *ppExpr += elseSize;
                }
            else
                {
                *ppExpr += thenSize;
                val = evalWord16Expr(ppExpr, context);
                }
            break;
        default:
            sendStringf("eW16E:O%d", exprOp);
        }
        return val;
    }

int16_t evalInt16Expr(byte **ppExpr, CONTEXT *context) 
    {
    byte *pExpr = *ppExpr;
    byte exprOp = *pExpr & EXPR_OP_MASK;
    int16_t val = 0;
    int16_t e1,e2,e3;
    uint8_t e8_1;
    bool conditional;
    uint16_t thenSize, elseSize;
    int refNum;
    byte bind;
    byte *bindPtr;

    switch (exprOp)
        {
        case EXPR_BIND:
            bind = pExpr[1];
            if (!context->bind)
                {
                val = 0;
                }
            else
                {
                bindPtr = &context->bind[bind * BIND_SPACING];
                val = evalInt16Expr(&bindPtr, context);
                }
            *ppExpr += 2; // Use Cmd and Bind Index bytes
            break;
        case EXPR_LIT:
            memcpy((byte *) &val, &pExpr[1], sizeof(uint16_t));
            *ppExpr += 1 + sizeof(uint16_t); // Use Cmd and Value bytes
            break;
        case EXPR_REF:
            refNum = pExpr[1];
            val = readRefInt16(refNum);
            *ppExpr += 2; // Use Cmd and Ref bytes
            break;
        case EXPR_NEG:
        case EXPR_SIGN:
        case EXPR_COMP:
            *ppExpr += 1; // Use command byte
            e1 = evalInt16Expr(ppExpr, context);
            if (exprOp == EXPR_NEG)
                val = -e1;
            else if (exprOp == EXPR_SIGN)
                {
                if (e1 < 0)
                    val = -1;
                else if (e1 == 0)
                    val = 0;
                else
                    val = 1;
                }
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
            e1 = evalInt16Expr(ppExpr, context);
            e2 = evalInt16Expr(ppExpr, context);
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
                    e3 = e1 % e2;
                    if ((e3 != 0) && ((e3 < 0) != (e2 < 0)))
                        val = e1 / e2 - 1;
                    else
                        val = e1 / e2;
                    break;
                case EXPR_REM:
                    val = e1 % e2;
                    break;
                }
            break;
        case EXPR_SHFL:
        case EXPR_SHFR:
        case EXPR_SETB:
        case EXPR_CLRB:
            *ppExpr += 1; // Use command byte
            e1 = evalInt16Expr(ppExpr, context);
            e8_1 = evalWord8Expr(ppExpr, context);
            switch(exprOp)
                {
                case EXPR_SHFL:
                    if (e8_1 > 16) e8_1 = 16;
                    val = e1 << e8_1;
                    break;
                case EXPR_SHFR:
                    if (e8_1 > 16) e8_1 = 16;
                    val = e1 >> e8_1;
                    break;
                case EXPR_SETB:
                    if (e8_1 > 15)
                        val = e1;
                    else
                        val = bitSet(e1, e8_1);
                    break;
                case EXPR_CLRB:
                    if (e8_1 > 15)
                        val = e1;
                    else
                        val = bitClear(e1, e8_1);
                    break;
                }
            break;
        case EXPR_FINT:
            *ppExpr += 1; // Use command byte
            val = evalWord32Expr(ppExpr, context);
            break;
        case EXPR_IF:
            memcpy((byte *) &thenSize, &pExpr[1], sizeof(uint16_t));
            memcpy((byte *) &elseSize, &pExpr[3], sizeof(uint16_t));
            *ppExpr += 1 + 2*sizeof(uint16_t); // Use Cmd and Value bytes
            conditional = evalBoolExpr(ppExpr, context);
            if (conditional)
                {
                val = evalInt16Expr(ppExpr, context);
                *ppExpr += elseSize;
                }
            else
                {
                *ppExpr += thenSize;
                val = evalInt16Expr(ppExpr, context);
                }
            break;
        default:
            sendStringf("eI16E:O%d", exprOp);
        }
        return val;
    }

uint32_t evalWord32Expr(byte **ppExpr, CONTEXT *context) 
    {
    byte *pExpr = *ppExpr;
    byte exprOp = *pExpr & EXPR_OP_MASK;
    uint32_t val = 0;
    uint32_t e1,e2;
    uint8_t e8_1;
    bool conditional;
    uint16_t thenSize, elseSize;
    byte exprType;
    int refNum;
    byte bind;
    byte *bindPtr;

    switch (exprOp)
        {
        case EXPR_BIND:
            bind = pExpr[1];
            if (!context->bind)
                {
                val = 0;
                }
            else
                {
                bindPtr = &context->bind[bind * BIND_SPACING];
                val = evalWord32Expr(&bindPtr, context);
                }
            *ppExpr += 2; // Use Cmd and Bind Index bytes
            break;
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
            e1 = evalWord32Expr(ppExpr, context);
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
            e1 = evalWord32Expr(ppExpr, context);
            e2 = evalWord32Expr(ppExpr, context);
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
        case EXPR_SETB:
        case EXPR_CLRB:
            *ppExpr += 1; // Use command byte
            e1 = evalWord32Expr(ppExpr, context);
            e8_1 = evalWord8Expr(ppExpr, context);
            switch(exprOp)
                {
                case EXPR_SHFL:
                    if (e8_1 > 32) e8_1 = 32;
                    val = e1 << e8_1;
                    break;
                case EXPR_SHFR:
                    if (e8_1 > 32) e8_1 = 32;
                    val = e1 >> e8_1;
                    break;
                case EXPR_SETB:
                    if (e8_1 > 31)
                        val = e1;
                    else
                        val = bitSet(e1, e8_1);
                    break;
                case EXPR_CLRB:
                    if (e8_1 > 31)
                        val = e1;
                    else
                        val = bitClear(e1, e8_1);
                    break;
                }
            break;
        case EXPR_TINT:
            exprType = *pExpr >> EXPR_TYPE_SHFT;
            *ppExpr += 1; // Use command byte
            if (exprType == EXPR_WORD8)            
                val = evalWord8Expr(ppExpr, context);
            else if (exprType == EXPR_WORD16)
                val = evalWord16Expr(ppExpr, context);
            else if (exprType == EXPR_INT8)            
                val = evalInt8Expr(ppExpr, context);
            else if (exprType == EXPR_INT16)
                val = evalInt16Expr(ppExpr, context);
            else if (exprType == EXPR_INT32)
                val = evalInt32Expr(ppExpr, context);
            else
                sendStringf("eW32E:T%d", exprType);
            break;
        case EXPR_IF:
            memcpy((byte *) &thenSize, &pExpr[1], sizeof(uint16_t));
            memcpy((byte *) &elseSize, &pExpr[3], sizeof(uint16_t));
            *ppExpr += 1 + 2*sizeof(uint16_t); // Use Cmd and Value bytes
            conditional = evalBoolExpr(ppExpr, context);
            if (conditional)
                {
                val = evalWord32Expr(ppExpr, context);
                *ppExpr += elseSize;
                }
            else
                {
                *ppExpr += thenSize;
                val = evalWord32Expr(ppExpr, context);
                }
            break;
        default:
            sendStringf("eW32E:O%d", exprOp);
        }
        return val;
    }

int32_t evalInt32Expr(byte **ppExpr, CONTEXT *context) 
    {
    byte *pExpr = *ppExpr;
    byte exprOp = *pExpr & EXPR_OP_MASK;
    int32_t val = 0;
    int32_t e1,e2,e3;
    uint8_t e8_1;
    bool conditional;
    uint16_t thenSize, elseSize;
    int refNum;
    byte bind;
    byte *bindPtr;

    switch (exprOp)
        {
        case EXPR_BIND:
            bind = pExpr[1];
            if (!context->bind)
                {
                val = 0;
                }
            else
                {
                bindPtr = &context->bind[bind * BIND_SPACING];
                val = evalInt32Expr(&bindPtr, context);
                }
            *ppExpr += 2; // Use Cmd and Bind Index bytes
            break;
        case EXPR_LIT:
            memcpy((byte *) &val, &pExpr[1], sizeof(uint32_t));
            *ppExpr += 1 + sizeof(uint32_t); // Use Cmd and Value bytes
            break;
        case EXPR_REF:
            refNum = pExpr[1];
            val = readRefInt32(refNum);
            *ppExpr += 2; // Use Cmd and Ref bytes
            break;
        case EXPR_NEG:
        case EXPR_SIGN:
        case EXPR_COMP:
            *ppExpr += 1; // Use command byte
            e1 = evalWord32Expr(ppExpr, context);
            if (exprOp == EXPR_NEG)
                val = -e1;
            else if (exprOp == EXPR_SIGN)
                {
                if (e1 < 0)
                    val = -1;
                else if (e1 == 0)
                    val = 0;
                else
                    val = 1;
                }
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
            e1 = evalInt32Expr(ppExpr, context);
            e2 = evalInt32Expr(ppExpr, context);
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
                    e3 = e1 % e2;
                    if ((e3 != 0) && ((e3 < 0) != (e2 < 0)))
                        val = e1 / e2 - 1;
                    else
                        val = e1 / e2;
                    break;
                case EXPR_REM:
                    val = e1 % e2;
                    break;
                }
            break;
        case EXPR_SHFR:
        case EXPR_SHFL:
        case EXPR_SETB:
        case EXPR_CLRB:
            *ppExpr += 1; // Use command byte
            e1 = evalInt32Expr(ppExpr, context);
            e8_1 = evalWord8Expr(ppExpr, context);
            switch(exprOp)
                {
                case EXPR_SHFL:
                    if (e8_1 > 32) e8_1 = 32;
                    val = e1 << e8_1;
                    break;
                case EXPR_SHFR:
                    if (e8_1 > 32) e8_1 = 32;
                    val = e1 >> e8_1;
                    break;
                case EXPR_SETB:
                    if (e8_1 > 31)
                        val = e1;
                    else
                        val = bitSet(e1, e8_1);
                    break;
                case EXPR_CLRB:
                    if (e8_1 > 31)
                        val = e1;
                    else
                        val = bitClear(e1, e8_1);
                    break;
                }
            break;
        case EXPR_IF:
            memcpy((byte *) &thenSize, &pExpr[1], sizeof(uint16_t));
            memcpy((byte *) &elseSize, &pExpr[3], sizeof(uint16_t));
            *ppExpr += 1 + 2*sizeof(uint16_t); // Use Cmd and Value bytes
            conditional = evalBoolExpr(ppExpr, context);
            if (conditional)
                {
                val = evalInt32Expr(ppExpr, context);
                *ppExpr += elseSize;
                }
            else
                {
                *ppExpr += thenSize;
                val = evalInt32Expr(ppExpr, context);
                }
            break;
        default:
            sendStringf("eW32E:O%d", exprOp);
        }
        return val;
    }

void putBindListPtr(CONTEXT *context, byte bind, byte *newPtr)
    {
    byte *bindPtr;
    byte *listPtr;

    if (context->bind)
        {
        bindPtr = &context->bind[bind * BIND_SPACING];
        memcpy(&listPtr, &bindPtr[1], sizeof(byte *));
        if (listPtr)
            free(listPtr);
        else
            memcpy(&bindPtr[1], &newPtr, sizeof(byte *));
        bindPtr[0] = EXPR(EXPR_LIST8, EXPR_PTR);
        }
    }

byte sizeList8Expr(byte **ppExpr, CONTEXT *context)
    {
    byte *pExpr = *ppExpr;
    byte exprOp = *pExpr & EXPR_OP_MASK;
    byte bind, refNum;
    byte *bindPtr, *refPtr;
    byte size = 0;

    switch (exprOp)
        {
        case EXPR_BIND:
            bind = pExpr[1];
            if (context->bind)
                {
                bindPtr = &context->bind[bind * BIND_SPACING];
                size = sizeList8Expr(&bindPtr, context);
                }
            *ppExpr += 2; // Use Cmd and Bind bytes
            break;
        case EXPR_PTR:
            memcpy(bindPtr, &pExpr[1], 4);
            size = sizeList8Expr(&bindPtr, context);
        case EXPR_REF:
            refNum = pExpr[1];
            refPtr = readRefList8(refNum);
            size = sizeList8Expr(&refPtr, context);
            *ppExpr += 2; // Use Cmd and Ref bytes
            break;
        case EXPR_LIT:
            size = pExpr[1];
            *ppExpr += 2 + size; // Use Cmd and size bytes, + list size
            break;
        case EXPR_PACK:
            size = pExpr[1];
            *ppExpr += 2; // Use Cmd and size bytes
            for (int ex = 0; ex < size; ex++)
                {
                evalWord8Expr(ppExpr, context);
                }
            break;
        case EXPR_APND:
            *ppExpr += 1; // Use command byte
            size = sizeList8Expr(ppExpr, context) + sizeList8Expr(ppExpr, context);
            break;
        case EXPR_CONS:
            *ppExpr += 1; // Use command byte
            evalWord8Expr(ppExpr, context);
            size = 1 + sizeList8Expr(ppExpr, context);
            break;
        default:
            sendStringf("sL8E:O%d", exprOp);
        }
        return size;
    }

int evalList8SubExpr(byte **ppExpr, CONTEXT *context, byte *listMem, byte index)
    {
    byte *pExpr = *ppExpr;
    byte exprOp = *pExpr & EXPR_OP_MASK;
    byte bind, refNum;
    int size;
    byte *bindPtr, *refPtr;

    switch (exprOp)
        {
        case EXPR_BIND:
            bind = pExpr[1];
            if (context->bind)
                {
                bindPtr = &context->bind[bind * BIND_SPACING];
                size = evalList8SubExpr(&bindPtr, context, listMem, index);
                }
            *ppExpr += 2; // Use Cmd and Bind bytes
            break;
        case EXPR_PTR:
            memcpy(bindPtr, &pExpr[1], 4);
            size = evalList8SubExpr(&bindPtr, context, listMem, index);
        case EXPR_REF:
            refNum = pExpr[1];
            refPtr = readRefList8(refNum);
            size = evalList8SubExpr(&refPtr, context, listMem, index);
            *ppExpr += 2; // Use Cmd and Ref bytes
            break;
        case EXPR_LIT:
            size = pExpr[1];
            memcpy(&listMem[index], &pExpr[2], size);
            *ppExpr += 2 + size; // Use Cmd and size bytes, + list size
            break;
        case EXPR_PACK:
            size = pExpr[1];
            *ppExpr += 2; // Use Cmd and size bytes
            for (int ex = 0; ex < size; ex++, index++)
                {
                listMem[index] = evalWord8Expr(ppExpr, context);
                }
            break;
        case EXPR_APND:
            *ppExpr += 1; // Use command byte
            size = evalList8SubExpr(ppExpr, context, listMem, index);
            size += evalList8SubExpr(ppExpr, context, listMem, index + size);
            break;
        case EXPR_CONS:
            *ppExpr += 1; // Use command byte
            listMem[index] = evalWord8Expr(ppExpr, context);
            size = evalList8SubExpr(ppExpr, context, listMem, index + 1) + 1;
            break;
        default:
            sendStringf("eL8SE:O%d", exprOp);
        }
    return size;
    }

uint8_t *evalList8Expr(byte **ppExpr, CONTEXT *context, bool *alloc)
    {
    byte *pExpr = *ppExpr;
    byte exprOp = *pExpr & EXPR_OP_MASK;
    byte *ppSizeExpr, *listMem, *bindPtr;
    byte size, bind, refNum;

    // If it is a literal, just return a pointer to the list
    if (exprOp == EXPR_LIT)
        {
        *alloc = false;
        listMem = pExpr;
        *ppExpr += 2 + listMem[1]; // Use command byte, length byte, and list
        }
    // Same case with a binding
    else if (exprOp == EXPR_BIND)
        {
        bind = pExpr[1];
        // If not context or bind space, allocate an empty list
        if (!context || !context->bind)
            {
            *alloc = true;
            listMem = (byte *) malloc(2);
            listMem[0] = EXPR(EXPR_LIST8, EXPR_LIT);
            listMem[1] = 0;
            }
        else
            {
            *alloc = false;
            bindPtr = &context->bind[bind * BIND_SPACING];
            memcpy(&listMem, &bindPtr[1], sizeof(byte *));
            }
        *ppExpr += 2; // Use command byte, byte byte
        }
    else if (exprOp == EXPR_REF)
        {
        *alloc = false;
        refNum = pExpr[1];
        listMem = readRefList8(refNum);
        *ppExpr += 2; // Use command byte, byte byte
        }
    else if (exprOp == EXPR_IF)
        {
        uint16_t thenSize, elseSize;
        bool conditional;

        memcpy((byte *) &thenSize, &pExpr[1], sizeof(uint16_t));
        memcpy((byte *) &elseSize, &pExpr[3], sizeof(uint16_t));
        *ppExpr += 1 + 2*sizeof(uint16_t); // Use Cmd and Value bytes
        conditional = evalBoolExpr(ppExpr, context);
        if (conditional)
            {
            listMem = evalList8Expr(ppExpr, context, alloc);
            *ppExpr += elseSize;
            }
        else
            {
            *ppExpr += thenSize;
            listMem = evalList8Expr(ppExpr, context, alloc);
            }
        }
    else
        {
        *alloc = true;
        ppSizeExpr = *ppExpr;
        size = sizeList8Expr(&ppSizeExpr, context); 
        
        listMem = (byte *) malloc(size+2);
        listMem[0] = EXPR(EXPR_LIST8, EXPR_LIT);
        listMem[1] = size;
        evalList8SubExpr(ppExpr, context, listMem, 2);
        }

    return listMem;
    }
