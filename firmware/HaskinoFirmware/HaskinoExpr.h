#ifndef HaskinoExprH
#define HaskinoExprH

#define EXPR_TYPE_MASK 0x0E
#define EXPR_TYPE_SHFT 5
#define EXPR_BOOL   0x01
#define EXPR_WORD8  0x02
#define EXPR_WORD16 0x03
#define EXPR_WORD32 0x04

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

bool evalBoolExpr(byte **ppExpr); 
uint8_t evalWord8Expr(byte **ppExpr); 
uint16_t evalWord16Expr(byte **ppExpr); 
uint32_t evalWord32Expr(byte **ppExpr);

#endif /* HaskinoExprH */
