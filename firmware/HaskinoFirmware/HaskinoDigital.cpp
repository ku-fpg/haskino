#include <Arduino.h>
#include "HaskinoComm.h"
#include "HaskinoCommands.h"
#include "HaskinoDigital.h"
#include "HaskinoExpr.h"

static bool handleReadPin(int size, const byte *msg, byte *local);
static bool handleWritePin(int size, const byte *msg);
static bool handleReadPinE(int size, const byte *msg, byte *local);
static bool handleWritePinE(int size, const byte *msg);

bool parseDigitalMessage(int size, const byte *msg, byte *local)
    {
    switch (msg[0]) 
        {
        case DIG_CMD_READ_PIN:
            handleReadPin(size, msg, local);
            break;
        case DIG_CMD_WRITE_PIN:
            handleWritePin(size, msg);
            break;
        case DIG_CMD_READ_PIN_E:
            handleReadPinE(size, msg, local);
            break;
        case DIG_CMD_WRITE_PIN_E:
            handleWritePinE(size, msg);
            break;
        }
    return false;
    }

static bool handleReadPin(int size, const byte *msg, byte *local)
    {
    byte pinNo = msg[1];
    byte digitalReply = digitalRead(pinNo);

    sendReply(sizeof(digitalReply), DIG_RESP_READ_PIN, &digitalReply, local);
    return false;
    }

static bool handleWritePin(int size, const byte *msg)
    {
    byte pinNo = msg[1];
    byte value = msg[2];

    digitalWrite(pinNo, value);
    return false;
    }

static bool handleReadPinE(int size, const byte *msg, byte *local)
    {
    byte *expr = (byte *) &msg[1];
    byte pinNo = evalWord8Expr(&expr);
    byte digitalReply = digitalRead(pinNo);

    sendReply(sizeof(digitalReply), DIG_RESP_READ_PIN, &digitalReply, local);
    return false;
    }

static bool handleWritePinE(int size, const byte *msg)
    {
    byte *expr = (byte *) &msg[1];
    byte pinNo = evalWord8Expr(&expr);
    byte value = evalBoolExpr(&expr);

    digitalWrite(pinNo, value);
    return false;
    }
