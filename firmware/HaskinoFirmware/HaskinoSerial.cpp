#include <Arduino.h>
#include "HaskinoComm.h"
#include "HaskinoCommands.h"
#include "HaskinoConfig.h"
#include "HaskinoExpr.h"
#include "HaskinoSerial.h"

#ifdef INCLUDE_SERIAL_CMDS
static bool handleBegin(int size, const byte *msg, CONTEXT *context);
static bool handleEnd(int size, const byte *msg, CONTEXT *context);
static bool handleWrite(int size, const byte *msg, CONTEXT *context);
static bool handleWriteList(int size, const byte *msg, CONTEXT *context);
static bool handleRead(int size, const byte *msg, CONTEXT *context);
static bool handleReadist(int size, const byte *msg, CONTEXT *context);

bool parseSerialMessage(int size, const byte *msg, CONTEXT *context)
    {
    switch (msg[0] ) 
        {
        case SER_CMD_BEGIN:
            return handleBegin(size, msg, context);
            break;
        case SER_CMD_END:
            return handleEnd(size, msg, context);
            break;
        case SER_CMD_WRITE:
            return handleWrite(size, msg, context);
            break;
        case SER_CMD_WRITE_LIST:
            return handleWriteList(size, msg, context);
            break;
        case SER_CMD_READ:
            return handleRead(size, msg, context);
            break;
        case SER_CMD_READ_LIST:
            return handleReadist(size, msg, context);
            break;
        }
    return false;
    }

static HardwareSerial *getDev(uint8_t p)
    {
    switch(p)
        {
        case 0:
            return &Serial;
        case 1:
            return &Serial1;
        case 2:
            return &Serial2;
        case 3:
            return &Serial3;
        default:
            return NULL;
        }
    }

static bool handleBegin(int size, const byte *msg, CONTEXT *context)
    {
    byte *expr = (byte *) &msg[1];
    byte port = evalWord8Expr(&expr, context);
    byte rate = evalWord32Expr(&expr, context);
    HardwareSerial *dev = getDev(port);

    if (dev)
        dev ->begin(rate);
    return false;
    }

static bool handleEnd(int size, const byte *msg, CONTEXT *context)
    {
    byte *expr = (byte *) &msg[1];
    byte port = evalWord8Expr(&expr, context);
    HardwareSerial *dev = getDev(port);

    if (dev)
        dev ->end();
    return false;
    }

static bool handleReadList(int size, const byte *msg, CONTEXT *context)
    {
    byte bind = msg[1];
    byte *expr = (byte *) &msg[2];
    byte port = evalWord8Expr(&expr, context);
    byte *localMem, *local;
    int byteAvail;

    Wire.requestFrom((int) slaveAddress, (int) byteCount);
    byteAvail = Wire.available();

    if (byteCount < byteAvail) 
        {
#ifdef DEBUG
        sendStringf("I2C: M");
#endif
        } 
    else if (byteCount > byteAvail) 
        {
#ifdef DEBUG
        sendStringf("I2C: F");
#endif
        }

    localMem = (byte *) malloc(byteAvail+3);
    local = &localMem[3];

    localMem[0] = EXPR_LIST8;
    localMem[1] = EXPR_LIT;
    localMem[2] = byteAvail;

    for (int i = 0; i < byteAvail; i++)
        { 
        *local++ = Wire.read();
        }

    if (context && context->bind)
        {
        putBindListPtr(context, bind, localMem);
        }
    else 
        {
        sendReply(byteAvail+3, I2C_RESP_READ, localMem, context, bind);
        free(localMem);    
        }
    return false;
    }

static bool handleWrite(int size, const byte *msg, CONTEXT *context)
    {
    byte *expr = (byte *) &msg[1];
    byte slaveAddress = evalWord8Expr(&expr, context);
    bool alloc;
    byte *list = evalList8Expr(&expr, context, &alloc);
    byte listSize = list[1];
    const byte *data = &list[2];
    byte byteCount = size;

    if (byteCount > listSize)
        byteCount = listSize;

    if (byteCount > 0)
        {
        Wire.beginTransmission(slaveAddress);
        Wire.write(data, byteCount);
        Wire.endTransmission();
        delayMicroseconds(70);
        }

    if (alloc)
        free(list);

    return false;
    }
#endif
