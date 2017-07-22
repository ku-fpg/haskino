#include <Arduino.h>
#include <Wire.h>
#include "HaskinoComm.h"
#include "HaskinoCommands.h"
#include "HaskinoConfig.h"
#include "HaskinoExpr.h"
#include "HaskinoI2C.h"

#ifdef INCLUDE_I2C_CMDS
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
    byte bind = msg[1];
    byte *expr = (byte *) &msg[2];
    byte slaveAddress = evalWord8Expr(&expr, context);
    byte byteCount = evalWord8Expr(&expr, context);
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
