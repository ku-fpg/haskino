#include <Arduino.h>
#include <Wire.h>
#include "AmberComm.h"
#include "AmberCommands.h"
#include "AmberI2C.h"

static int handleRead(int size, byte *msg);
static int handleReadReg(int size, byte *msg);
static int handleWrite(int size, byte *msg);

int parseI2CMessage(int size, byte *msg)
    {
    switch (msg[0] ) 
        {
        case I2C_CMD_READ:
            return handleRead(size, msg);
            break;
        case I2C_CMD_READ_REG:
            return handleReadReg(size, msg);
            break;
        case I2C_CMD_WRITE:
            return handleWrite(size, msg);
            break;
        }
    }

static int readFrom(byte address, byte wordCount)
    {
    int byteAvail;

    Wire.requestFrom((int) address, (int) wordCount*2);
    byteAvail = Wire.available();

    startReplyFrame(I2C_RESP_READ);

    for (int i = 0; i < byteAvail; i++) 
        {
        sendReplyByte(Wire.read());
        }

    endReplyFrame();    
    }

static int handleRead(int size, byte *msg)
    {
    byte slaveAddress = msg[1];
    byte wordCount = msg[2];

    readFrom(slaveAddress, wordCount);
    return 3;
    }

static int handleReadReg(int size, byte *msg)
    {
    byte slaveAddress = msg[1];
    uint16_t slaveRegister = msg[2] + msg[3] << 8;
    byte wordCount = msg[3];

    Wire.beginTransmission(slaveAddress);
    Wire.write(slaveRegister); // TBD size and byte order
    Wire.endTransmission();
    delayMicroseconds(70);

    readFrom(slaveAddress, wordCount);
    return 4;
    }

static int handleWrite(int size, byte *msg)
    {
    byte slaveAddress = msg[1];
    byte wordCount = msg[2];
    uint16_t *data = (uint16_t *) &msg[3];

    if (wordCount > (size - 2) / 2)
        wordCount = (size - 2) / 2;

    Wire.beginTransmission(slaveAddress);
    for (int i = 0; i < wordCount; i++) 
        {
        Wire.write(*data++);
        }
    Wire.endTransmission();
    delayMicroseconds(70);
    return 3 + wordCount*2;
    }
