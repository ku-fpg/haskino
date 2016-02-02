#include <Arduino.h>
#include "HaskinoOneWire.h"
#include "HaskinoConfig.h"

#ifdef INCLUDE_ONEW_CMDS
bool parseOneWireMessage(int size, const byte *msg, CONTEXT *context)
    {
    return false;
    }
#endif
