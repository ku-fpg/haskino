#include <Arduino.h>
#include "HaskinoComm.h"
#include "HaskinoCommands.h"
#include "HaskinoDigital.h"
#include "HaskinoExpr.h"

static bool handleReadPin(int size, const byte *msg, byte *local);
static bool handleWritePin(int size, const byte *msg, byte *local);
static bool handleReadPinE(int size, const byte *msg, byte *local);
static bool handleWritePinE(int size, const byte *msg, byte *local);

bool parseDigitalMessage(int size, const byte *msg, byte *local)
    {
    switch (msg[0]) 
        {
        case DIG_CMD_READ_PIN:
            handleReadPin(size, msg, local);
            break;
        case DIG_CMD_WRITE_PIN:
            handleWritePin(size, msg, local);
            break;
        case DIG_CMD_READ_PIN_E:
            handleReadPinE(size, msg, local);
            break;
        case DIG_CMD_WRITE_PIN_E:
            handleWritePinE(size, msg, local);
            break;
        }
    return false;
    }

static bool handleReadPin(int size, const byte *msg, byte *local)
    {
    byte pinNo = msg[1];
#ifdef INTEL_EDISON
    byte digitalReply = gpio_read(pinNo);
#else
    byte digitalReply = digitalRead(pinNo);
#endif

    sendReply(sizeof(digitalReply), DIG_RESP_READ_PIN, &digitalReply, local);
    return false;
    }

static bool handleWritePin(int size, const byte *msg, byte *local)
    {
    byte pinNo = msg[1];
    byte value = msg[2];

#ifdef INTEL_EDISON
    gpio_write(pinNo, value);
#else
    digitalWrite(pinNo, value);
#endif
    return false;
    }

static bool handleReadPinE(int size, const byte *msg, byte *local)
    {
    byte *expr = (byte *) &msg[1];
    byte pinNo = evalWord8ExprOrBind(&expr, local);
    byte digitalReply[2];

    digitalReply[0] = EXPR(EXPR_WORD8, EXPR_LIT);
    digitalReply[1] = digitalRead(pinNo);

    sendReply(sizeof(digitalReply), DIG_RESP_READ_PIN, digitalReply, local);
    return false;
    }

static bool handleWritePinE(int size, const byte *msg, byte *local)
    {
    byte *expr = (byte *) &msg[1];
    byte pinNo = evalWord8ExprOrBind(&expr, local);
    byte value = evalBoolExprOrBind(&expr, local);

    digitalWrite(pinNo, value);
    return false;
    }
