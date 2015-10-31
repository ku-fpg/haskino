#ifndef HaskinoRefsH
#define HaskinoRefsH

#include "HaskinoScheduler.h"

bool parseRefMessage(int size, const byte *msg, CONTEXT *context);
bool readRefBool(int refIndex);
uint8_t readRefWord8(int refIndex);
uint16_t readRefWord16(int refIndex);
uint32_t readRefWord32(int refIndex);
uint8_t *readRefList8(int refIndex);
void storeBoolRef(byte *expr, CONTEXT *context, byte refIndex);
void storeWord8Ref(byte *expr, CONTEXT *context, byte refIndex);
void storeWord16Ref(byte *expr, CONTEXT *context, byte refIndex);
void storeWord32Ref(byte *expr, CONTEXT *context, byte refIndex);
void storeList8Ref(byte *expr, CONTEXT *context, byte refIndex);

#endif /* HaskinoRefsH */
