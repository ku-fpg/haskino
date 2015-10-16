#include <Arduino.h>
#include <Wire.h>
#include "HaskinoComm.h"
#include "HaskinoCommands.h"
#include "HaskinoExpr.h"
#include "HaskinoI2C.h"

static bool handleConfig(int size, const byte *msg, CONTEXT *context);
static bool handleRead(int size, const byte *msg, CONTEXT *context);
static bool handleWrite(int size, const byte *msg, CONTEXT *context);

bool parseI2CMessage(int size, const byte *msg, CONTEXT *context)
    {
    switch (msg[0] ) 
        {
        case I2C_CMD_CONFIG:
            return handleConfig(size, msg, context);
            break;
        case I2C_CMD_READ:
            return handleRead(size, msg, context);
            break;
        case I2C_CMD_WRITE:
            return handleWrite(size, msg, context);
            break;
        }
    return false;
    }

static bool handleConfig(int size, const byte *msg, CONTEXT *context)
    {
    Wire.begin();
    delay(10);
    return false;
    }

static bool handleRead(int size, const byte *msg, CONTEXT *context)
    {
    byte slaveAddress = msg[1];
    byte byteCount = msg[2];
    int byteAvail;

    Wire.requestFrom((int) slaveAddress, (int) byteCount);
    byteAvail = Wire.available();

    if (byteCount < byteAvail) 
        {
        sendStringf("I2C: Too many bytes received");
        } 
    else if (byteCount > byteAvail) 
        {
        sendStringf("I2C: Too few bytes received");
        }

    if (context->bind)
        {
        byte* local = context->bind;
        for (int i = 0; i < byteAvail; i++)
            { 
            *local++ = Wire.read();
            }
        }
    else 
        {
        startReplyFrame(I2C_RESP_READ);

        for (int i = 0; i < byteAvail; i++) 
            {
            sendReplyByte(Wire.read());
            }

        endReplyFrame();    
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

    return false;
    }
