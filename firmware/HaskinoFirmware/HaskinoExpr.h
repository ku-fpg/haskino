#ifndef HaskinoExprH
#define HaskinoExprH

#include "HaskinoScheduler.h"

// Base Expression Types
#define EXPR_TYPE_MASK      0x7F
#define EXPR_UNIT           0x00
#define EXPR_BOOL           0x01
#define EXPR_WORD8          0x02
#define EXPR_WORD16         0x03
#define EXPR_WORD32         0x04
#define EXPR_INT8           0x05
#define EXPR_INT16          0x06
#define EXPR_INT32          0x07
#define EXPR_LIST8          0x08
#define EXPR_FLOAT          0x09

// Either Expression Types
#define EXPR_EITHER_MASK    0x80
#define EXPR_RIGHT          0x00
#define EXPR_LEFT           0x80

// Base Expression Ops
#define EXPR_LIT            0x00
#define EXPR_REF            0x01
#define EXPR_BIND           0x02
#define EXPR_EQ             0x03
#define EXPR_LESS           0x04
#define EXPR_IF             0x05
#define EXPR_FINT           0x06
#define EXPR_NEG            0x07
#define EXPR_SIGN           0x08
#define EXPR_ADD            0x09
#define EXPR_SUB            0x0A
#define EXPR_MULT           0x0B
#define EXPR_DIV            0x0C
#define EXPR_SHOW           0x0D
#define EXPR_NOT            0x0E
#define EXPR_AND            0x0F
#define EXPR_OR             0x10
#define EXPR_TINT           0x11
#define EXPR_XOR            0x12
#define EXPR_REM            0x13
#define EXPR_COMP           0x14
#define EXPR_SHFL           0x15
#define EXPR_SHFR           0x16
#define EXPR_TSTB           0x17
#define EXPR_SETB           0x18
#define EXPR_CLRB           0x19
#define EXPR_QUOT           0x1A
#define EXPR_MOD            0x1B

// List Expression Ops
#define EXPRL_ELEM          0x06
#define EXPRL_LEN           0x07
#define EXPRL_CONS          0x08
#define EXPRL_APND          0x09
#define EXPRL_PACK          0x0A
#define EXPRL_PTR           0x0B

// Float Expression Ops
#define EXPRF_TRUNC         0x0E
#define EXPRF_FRAC          0x0F
#define EXPRF_ROUND         0x10
#define EXPRF_CEIL          0x11
#define EXPRF_FLOOR         0x12
#define EXPRF_PI            0x13
#define EXPRF_EXP           0x14
#define EXPRF_LOG           0x15
#define EXPRF_SQRT          0x16
#define EXPRF_SIN           0x17
#define EXPRF_COS           0x18
#define EXPRF_TAN           0x19
#define EXPRF_ASIN          0x1A
#define EXPRF_ACOS          0x1B
#define EXPRF_ATAN          0x1C
#define EXPRF_ATAN2         0x1D
#define EXPRF_SINH          0x1E
#define EXPRF_COSH          0x1F
#define EXPRF_TANH          0x20
#define EXPRF_POWER         0x21
#define EXPRF_ISNAN         0x22
#define EXPRF_ISINF         0x23

bool evalBoolExpr(byte **ppExpr, CONTEXT *context);
uint8_t evalWord8Expr(byte **ppExpr, CONTEXT *context);
uint16_t evalWord16Expr(byte **ppExpr, CONTEXT *context);
uint32_t evalWord32Expr(byte **ppExpr, CONTEXT *context);
uint8_t *evalList8Expr(byte **ppExpr, CONTEXT *context, bool *alloc);
int8_t evalInt8Expr(byte **ppExpr, CONTEXT *context);
int16_t evalInt16Expr(byte **ppExpr, CONTEXT *context);
int32_t evalInt32Expr(byte **ppExpr, CONTEXT *context);
float evalFloatExpr(byte **ppExpr, CONTEXT *context);
bool parseExprMessage(int size, const byte *msg, CONTEXT *context);
void putBindListPtr(CONTEXT *context, byte bind, byte *newPtr);
void storeBoolBind(byte *expr, CONTEXT *context, byte bind);
void storeWord8Bind(byte *expr, CONTEXT *context, byte bind);
void storeWord16Bind(byte *expr, CONTEXT *context, byte bind);
void storeWord32Bind(byte *expr, CONTEXT *context, byte bind);
void storeInt8Bind(byte *expr, CONTEXT *context, byte bind);
void storeInt16Bind(byte *expr, CONTEXT *context, byte bind);
void storeInt32Bind(byte *expr, CONTEXT *context, byte bind);
void storeFloatBind(byte *expr, CONTEXT *context, byte bind);
void storeList8Bind(byte *expr, CONTEXT *context, byte bind);

#endif /* HaskinoExprH */
