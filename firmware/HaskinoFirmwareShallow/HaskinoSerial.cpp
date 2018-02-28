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
static bool handleReadList(int size, const byte *msg, CONTEXT *context);

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
            return handleReadList(size, msg, context);
            break;
        }
    return false;
    }

static HardwareSerial *getDev(uint8_t p)
    {
    switch(p)
        {
#ifdef HAVE_HWSERIAL0
        case 0:
            return &Serial;
#endif
#ifdef HAVE_HWSERIAL1
        case 1:
            return &Serial1;
#endif
#ifdef HAVE_HWSERIAL2
        case 2:
            return &Serial2;
#endif
#ifdef HAVE_HWSERIAL3
        case 3:
            return &Serial3;
#endif
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

static bool handleRead(int size, const byte *msg, CONTEXT *context)
    {
    byte bind = msg[1];
    byte *expr = (byte *) &msg[2];
    byte port = evalWord8Expr(&expr, context);
    HardwareSerial *dev = getDev(port);
    byte readReply[6];
    int32_t data;

    if (dev)
        {
        data = dev->read();
        }
    else
        {
        data = -1;
        }

    readReply[0] = EXPR_INT32;
    readReply[1] = EXPR_LIT;
    memcpy(&readReply[2], &data, sizeof(data));

    sendReply(2 + sizeof(readReply), SER_RESP_READ, readReply, context, bind);

    return false;
    }

static bool handleReadList(int size, const byte *msg, CONTEXT *context)
    {
    byte bind = msg[1];
    byte *expr = (byte *) &msg[2];
    byte port = evalWord8Expr(&expr, context);
    HardwareSerial *dev = getDev(port);
    byte *localMem, *local;
    int byteAvail;

    if (dev)
        {
        byteAvail = dev->available();
        }
    else
        {
        byteAvail = 0;
        }

    localMem = (byte *) malloc(byteAvail+3);
    local = &localMem[3];

    localMem[0] = EXPR_LIST8;
    localMem[1] = EXPR_LIT;
    localMem[2] = byteAvail;

    for (int i = 0; i < byteAvail; i++)
        {
        *local++ = dev->read();
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
    byte port = evalWord8Expr(&expr, context);
    HardwareSerial *dev = getDev(port);
    byte data = evalWord8Expr(&expr, context);

    if (dev)
        {
        dev->write(data);
        }

    return false;
    }

static bool handleWriteList(int size, const byte *msg, CONTEXT *context)
    {
    byte *expr = (byte *) &msg[1];
    byte port = evalWord8Expr(&expr, context);
    HardwareSerial *dev = getDev(port);
    bool alloc;
    byte *list = evalList8Expr(&expr, context, &alloc);
    byte listSize = list[1];
    const byte *data = &list[2];
    byte byteCount = size;

    if (byteCount > listSize)
        byteCount = listSize;

    if (dev && byteCount > 0)
        {
        dev->write(data, byteCount);
        }

    if (alloc)
        free(list);

    return false;
    }
#endif
