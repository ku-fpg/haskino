#include <Arduino.h>
#include "HaskinoComm.h"
#include "HaskinoCommands.h"
#include "HaskinoDigital.h"
#include "HaskinoExpr.h"

static bool handleReadPin(int size, const byte *msg, byte *local);
static bool handleWritePin(int size, const byte *msg, byte *local);

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
        }
    return false;
    }

static bool handleReadPin(int size, const byte *msg, byte *local)
    {
    byte bind = msg[1];
    byte *expr = (byte *) &msg[2];
    byte pinNo = evalWord8Expr(&expr, local);
    byte digitalReply[2];

    digitalReply[0] = EXPR(EXPR_WORD8, EXPR_LIT);
    digitalReply[1] = digitalRead(pinNo);

    sendReply(sizeof(digitalReply), DIG_RESP_READ_PIN, 
              digitalReply, local, bind);
    return false;
    }

static bool handleWritePin(int size, const byte *msg, byte *local)
    {
    byte *expr = (byte *) &msg[1];
    byte pinNo = evalWord8Expr(&expr, local);
    byte value = evalBoolExpr(&expr, local);

    digitalWrite(pinNo, value);
    return false;
    }
