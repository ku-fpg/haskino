#ifndef HaskinoI2CH
#define HaskinoI2CH

#include "HaskinoScheduler.h"

bool parseI2CMessage(int size, const byte *msg, CONTEXT *context);

#endif /* HaskinoI2CH */
