#ifndef HaskinoRefsH
#define HaskinoRefsH

#include "HaskinoScheduler.h"

#define REF_BOOL   0x00
#define REF_WORD8  0x01
#define REF_WORD16 0x02
#define REF_WORD32 0x03
#define REF_INT8   0x04
#define REF_INT16  0x05
#define REF_INT32  0x06
#define REF_LIST8  0x07
#define REF_FLOAT  0x08

bool parseRefMessage(int size, const byte *msg, CONTEXT *context);
bool readRefBool(int refIndex);
uint8_t readRefWord8(int refIndex);
uint16_t readRefWord16(int refIndex);
uint32_t readRefWord32(int refIndex);
uint8_t *readRefList8(int refIndex);
int8_t readRefInt8(int refIndex);
int16_t readRefInt16(int refIndex);
int32_t readRefInt32(int refIndex);
float readRefFloat(int refIndex);
byte *storeBoolRef(byte *expr, CONTEXT *context, byte refIndex);
byte *storeWord8Ref(byte *expr, CONTEXT *context, byte refIndex);
byte *storeWord16Ref(byte *expr, CONTEXT *context, byte refIndex);
byte *storeWord32Ref(byte *expr, CONTEXT *context, byte refIndex);
byte *storeList8Ref(byte *expr, CONTEXT *context, byte refIndex);
byte *storeInt8Ref(byte *expr, CONTEXT *context, byte refIndex);
byte *storeInt16Ref(byte *expr, CONTEXT *context, byte refIndex);
byte *storeInt32Ref(byte *expr, CONTEXT *context, byte refIndex);
byte *storeFloatRef(byte *expr, CONTEXT *context, byte refIndex);

#endif /* HaskinoRefsH */
