#include <Arduino.h>
#include <Wire.h>
#include "HaskinoComm.h"
#include "HaskinoCommands.h"
#include "HaskinoConfig.h"
#include "HaskinoI2C.h"

#ifdef INCLUDE_I2C_CMDS
static void handleConfig(int size, const byte *msg);
static void handleRead(int size, const byte *msg);
static void handleWrite(int size, const byte *msg);

void parseI2CMessage(int size, const byte *msg)
    {
    switch (msg[0] )
        {
        case I2C_CMD_CONFIG:
            return handleConfig(size, msg);
            break;
        case I2C_CMD_READ:
            return handleRead(size, msg);
            break;
        case I2C_CMD_WRITE:
            return handleWrite(size, msg);
            break;
        }
    }

static void handleConfig(int size, const byte *msg)
    {
    Wire.begin();
    delay(10);
    }

static void handleRead(int size, const byte *msg)
    {
    byte slaveAddress;
    byte byteCount;
    byte *localMem, *local;
    int byteAvail;

    if ( msg[1] == EXPR_WORD8  && msg[2] == EXPR_LIT &&
         msg[4] == EXPR_BOOL   && msg[5] == EXPR_LIT )
        {
        slaveAddress = msg[3];
        byteCount = msg[6];
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

        sendReply(byteAvail+3, I2C_RESP_READ, localMem);
        free(localMem);
        }
    }

static void handleWrite(int size, const byte *msg)
    {
    byte slaveAddress;
    byte *list;
    byte listSize;
    const byte *data;
    byte byteCount;

    if ( msg[1] == EXPR_WORD8  && msg[2] == EXPR_LIT &&
         msg[4] == EXPR_LIST8  && msg[5] == EXPR_LIT )
        {
        slaveAddress = msg[3];
        list = (byte *) &msg[6];

        listSize = list[1];
        data = &list[2];
        byteCount = size;

        if (byteCount > listSize)
            byteCount = listSize;

        if (byteCount > 0)
            {
            Wire.beginTransmission(slaveAddress);
            Wire.write(data, byteCount);
            Wire.endTransmission();
            delayMicroseconds(70);
            }
        }
    }
#endif
