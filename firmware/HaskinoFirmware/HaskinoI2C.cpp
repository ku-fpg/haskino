#include <Arduino.h>
#include <Wire.h>
#include "HaskinoComm.h"
#include "HaskinoCommands.h"
#include "HaskinoI2C.h"

static bool handleConfig(int size, const byte *msg);
static bool handleRead(int size, const byte *msg, byte *local);
static bool handleWrite(int size, const byte *msg);

bool parseI2CMessage(int size, const byte *msg, byte *local)
    {
    switch (msg[0] ) 
        {
        case I2C_CMD_CONFIG:
            return handleConfig(size, msg);
            break;
        case I2C_CMD_READ:
            return handleRead(size, msg, local);
            break;
        case I2C_CMD_WRITE:
            return handleWrite(size, msg);
            break;
        }
    return false;
    }

static bool handleConfig(int size, const byte *msg)
    {
    Wire.begin();
    delay(10);
    return false;
    }

static bool handleRead(int size, const byte *msg, byte *local)
    {
    byte slaveAddress = msg[1];
    byte byteCount = msg[2];
    int byteAvail;

    Wire.requestFrom((int) slaveAddress, (int) byteCount);
    byteAvail = Wire.available();

    if (byteCount < byteAvail) 
        {
        sendString("I2C: Too many bytes received");
        } 
    else if (byteCount > byteAvail) 
        {
        sendString("I2C: Too few bytes received");
        }

    if (local)
        {
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

static bool handleWrite(int size, const byte *msg)
    {
    byte slaveAddress = msg[1];
    const byte *data = &msg[2];
    byte byteCount = size - 2;

    if (byteCount > 0)
        {
        Wire.beginTransmission(slaveAddress);
        Wire.write(data, byteCount);
        Wire.endTransmission();
        delayMicroseconds(70);
        }

    return false;
    }
