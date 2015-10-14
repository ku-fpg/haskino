#ifndef HaskinoExprH
#define HaskinoExprH

#include "HaskinoScheduler.h"

#define EXPR(a,b) ((a << EXPR_TYPE_SHFT) | b)

#define EXPR_TYPE_MASK 0xE0
#define EXPR_TYPE_SHFT 5
#define EXPR_BOOL   0x01
#define EXPR_WORD8  0x02
#define EXPR_WORD16 0x03
#define EXPR_WORD32 0x04
#define EXPR_LIST8  0x05

#define EXPR_OP_MASK 0x1F
#define EXPR_LIT  0x00
#define EXPR_REF  0x01
#define EXPR_NOT  0x02
#define EXPR_AND  0x03
#define EXPR_OR   0x04
#define EXPR_XOR  0x05
#define EXPR_NEG  0x06
#define EXPR_SIGN 0x07
#define EXPR_ADD  0x08
#define EXPR_SUB  0x09
#define EXPR_MULT 0x0A
#define EXPR_DIV  0x0B
#define EXPR_REM  0x0C
#define EXPR_COMP 0x0D
#define EXPR_SHFL 0x0E
#define EXPR_SHFR 0x0F
#define EXPR_EQ   0x10
#define EXPR_LESS 0x11
#define EXPR_IF   0x12
#define EXPR_BIT  0x13
#define EXPR_SETB 0x14
#define EXPR_CLRB 0x15
#define EXPR_TSTB 0x16
#define EXPR_BIND 0x17
#define EXPR_ELEM 0x18
#define EXPR_CONS 0x19
#define EXPR_APND 0x1A

bool evalBoolExpr(byte **ppExpr, CONTEXT *context);
uint8_t evalWord8Expr(byte **ppExpr, CONTEXT *context);
uint16_t evalWord16Expr(byte **ppExpr, CONTEXT *context);
uint32_t evalWord32Expr(byte **ppExpr, CONTEXT *context);
uint8_t *evalList8Expr(byte **ppExpr, CONTEXT *context, byte bind);

#endif /* HaskinoExprH */
