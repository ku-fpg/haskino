#include <Arduino.h>
#include "HaskinoComm.h"
#include "HaskinoCommands.h"
#include "HaskinoDigital.h"
#include "HaskinoExpr.h"

static bool handleReadPin(int size, const byte *msg, CONTEXT *context);
static bool handleWritePin(int size, const byte *msg, CONTEXT *context);
static bool handleReadPort(int size, const byte *msg, CONTEXT *context);
static bool handleWritePort(int size, const byte *msg, CONTEXT *context);

bool parseDigitalMessage(int size, const byte *msg, CONTEXT *context)
    {
    switch (msg[0]) 
        {
        case DIG_CMD_READ_PIN:
            handleReadPin(size, msg, context);
            break;
        case DIG_CMD_WRITE_PIN:
            handleWritePin(size, msg, context);
            break;
        case DIG_CMD_READ_PORT:
            handleReadPort(size, msg, context);
            break;
        case DIG_CMD_WRITE_PORT:
            handleWritePort(size, msg, context);
            break;
        }
    return false;
    }

static bool handleReadPin(int size, const byte *msg, CONTEXT *context)
    {
    byte bind = msg[1];
    byte *expr = (byte *) &msg[2];
    byte pinNo = evalWord8Expr(&expr, context);
    byte digitalReply[2];

    digitalReply[0] = EXPR(EXPR_WORD8, EXPR_LIT);
    digitalReply[1] = digitalRead(pinNo);

    sendReply(sizeof(digitalReply), DIG_RESP_READ_PIN, 
              digitalReply, context, bind);
    return false;
    }

static bool handleWritePin(int size, const byte *msg, CONTEXT *context)
    {
    byte *expr = (byte *) &msg[1];
    byte pinNo = evalWord8Expr(&expr, context);
    byte value = evalBoolExpr(&expr, context);

    digitalWrite(pinNo, value);
    return false;
    }

static uint8_t bits[8] = {0x01,0x02,0x04,0x08,0x10,0x20,0x40,0x80};

static bool handleReadPort(int size, const byte *msg, CONTEXT *context)
    {
    byte bind = msg[1];
    byte *expr = (byte *) &msg[2];
    byte pinNo = evalWord8Expr(&expr, context);
    byte mask = evalWord8Expr(&expr, context);
    byte digitalReply[2];

    digitalReply[0] = EXPR(EXPR_WORD8, EXPR_LIT);
    digitalReply[1] = 0;
    for (int i=0;i<8;i++)
        {
        if ((bits[i] & mask) && digitalRead(pinNo+i))
            digitalReply[1] |= bits[i];
        }

    sendReply(sizeof(digitalReply), DIG_RESP_READ_PORT, 
              digitalReply, context, bind);
    return false;
    }

static bool handleWritePort(int size, const byte *msg, CONTEXT *context)
    {
    byte *expr = (byte *) &msg[1];
    byte pinNo = evalWord8Expr(&expr, context);
    byte value = evalWord8Expr(&expr, context);
    byte mask = evalWord8Expr(&expr, context);

    for (int i=0;i<8;i++)
        {
        if (bits[i] & mask)
            digitalWrite(pinNo+i, bits[i] & value);
        }

    return false;
    }
