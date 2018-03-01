#include <Arduino.h>
#include "HaskinoComm.h"
#include "HaskinoCommands.h"
#include "HaskinoConfig.h"
#include "HaskinoSerial.h"

#ifdef INCLUDE_SERIAL_CMDS
static void handleBegin(int size, const byte *msg);
static void handleEnd(int size, const byte *msg);
static void handleWrite(int size, const byte *msg);
static void handleWriteList(int size, const byte *msg);
static void handleRead(int size, const byte *msg);
static void handleReadList(int size, const byte *msg);

void parseSerialMessage(int size, const byte *msg)
    {
    switch (msg[0] )
        {
        case SER_CMD_BEGIN:
            handleBegin(size, msg);
            break;
        case SER_CMD_END:
            handleEnd(size, msg);
            break;
        case SER_CMD_WRITE:
            handleWrite(size, msg);
            break;
        case SER_CMD_WRITE_LIST:
            handleWriteList(size, msg);
            break;
        case SER_CMD_READ:
            handleRead(size, msg);
            break;
        case SER_CMD_READ_LIST:
            handleReadList(size, msg);
            break;
        }
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

static void handleBegin(int size, const byte *msg)
    {
    byte port;
    uint32_t rate;
    HardwareSerial *dev;

    if ( msg[1] == EXPR_WORD8  && msg[2] == EXPR_LIT &&
         msg[4] == EXPR_WORD32 && msg[5] == EXPR_LIT )
        {
        port = msg[3];
        rate = ((uint32_t) msg[9] << 24) | ((uint32_t) msg[8] << 16) |
               ((uint32_t) msg[7] << 8) | (uint32_t) msg[6];
        dev = getDev(port);

        if (dev)
            dev ->begin(rate);
        }
    }

static void handleEnd(int size, const byte *msg)
    {
    byte port;
    HardwareSerial *dev;

    if ( msg[1] == EXPR_WORD8 && msg[2] == EXPR_LIT)
        {
        port = msg[3];
        dev = getDev(port);

        if (dev)
            dev ->end();
        }
    }

static void handleRead(int size, const byte *msg)
    {
    byte port;
    HardwareSerial *dev;
    byte readReply[6];
    int32_t data;

    if ( msg[2] == EXPR_WORD8 && msg[3] == EXPR_LIT)
        {
        port = msg[4];
        dev = getDev(port);

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

        sendReply(2 + sizeof(readReply), SER_RESP_READ, readReply);
        }
    }

static void handleReadList(int size, const byte *msg)
    {
    byte port;
    HardwareSerial *dev;
    byte *localMem, *local;
    int byteAvail;

    if ( msg[2] == EXPR_WORD8 && msg[3] == EXPR_LIT)
        {
        port = msg[4];
        dev = getDev(port);

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

        sendReply(byteAvail+3, I2C_RESP_READ, localMem);
        free(localMem);
        }
    }

static void handleWrite(int size, const byte *msg)
    {
    byte port;
    HardwareSerial *dev;
    byte data;

    if ( msg[1] == EXPR_WORD8 && msg[2] == EXPR_LIT &&
         msg[4] == EXPR_WORD8 && msg[5] == EXPR_LIT )
        {
        port = msg[3];
        data = msg[6];
        dev = getDev(port);

        if (dev)
            {
            dev->write(data);
            }
        }
    }

static void handleWriteList(int size, const byte *msg)
    {
    byte port;
    HardwareSerial *dev;
    byte *list;
    byte listSize;
    const byte *data;
    byte byteCount;

    if ( msg[1] == EXPR_WORD8 && msg[2] == EXPR_LIT &&
         msg[4] == EXPR_LIST8 && msg[5] == EXPR_LIT )
        {
        port = msg[3];
        dev = getDev(port);

        list = (byte *) &msg[6];
        listSize = list[1];
        data = &list[2];
        byteCount = size;

        if (byteCount > listSize)
            byteCount = listSize;

        if (dev && byteCount > 0)
            {
            dev->write(data, byteCount);
            }
        }
    }
#endif
