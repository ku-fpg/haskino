#include <Arduino.h>
#include <Wire.h>
#include "AmberComm.h"
#include "AmberCommands.h"
#include "AmberI2C.h"

static bool handleConfig(int size, byte *msg);
static bool handleRead(int size, byte *msg);
static bool handleWrite(int size, byte *msg);

bool parseI2CMessage(int size, byte *msg)
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
    return false;
    }

static bool handleConfig(int size, byte *msg)
    {
    Wire.begin();
    delay(10);
    return false;
    }

static bool handleRead(int size, byte *msg)
    {
    byte slaveAddress = msg[1];
    byte byteCount = msg[2];
    int byteAvail;

    Wire.requestFrom((int) slaveAddress, (int) byteCount);
    byteAvail = Wire.available();

    startReplyFrame(I2C_RESP_READ);

    for (int i = 0; i < byteAvail; i++) 
        {
        sendReplyByte(Wire.read());
        }

    endReplyFrame();    
    return false;
    }

static bool handleWrite(int size, byte *msg)
    {
    byte slaveAddress = msg[1];
    byte *data = &msg[2];
    byte byteCount = size - 3;

    if (byteCount > 0)
        {
        Wire.beginTransmission(slaveAddress);
        Wire.write(data, byteCount);
        Wire.endTransmission();
        delayMicroseconds(170);
        }

    return false;
    }
