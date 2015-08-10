#include <Arduino.h>
#include <Wire.h>
#include "AmberCommands.h"
#include "AmberI2C.h"

static void handleRead(int size, byte *msg);
static void handleReadReg(int size, byte *msg);
static void handleWrite(int size, byte *msg);

void parseI2CMessage(int size, byte *msg)
    {
    switch (msg[0] ) 
        {
        case I2C_CMD_READ:
            handleRead(size, msg);
            break;
        case I2C_CMD_READ_REG:
            handleReadReg(size, msg);
            break;
        case I2C_CMD_WRITE:
            handleWrite(size, msg);
            break;
        }
    }

static void readFrom()
    {

    }

static void handleRead(int size, byte *msg)
    {
#if 0
    byte slaveAddress = msg[1];
    byte wordCount = msg[2];

    sendReply(sizeof(i2cReply), I2C_RESP_READ, i2cReply);
#endif
    }

static void handleReadReg(int size, byte *msg)
    {
#if 0
    byte slaveAddress = msg[1];
    uint16_t slaveRegister = msg[2] + msg[3] << 8;
    byte wordCount = msg[3];

    Wire.beginTransmission(slaveAddress);
    Wire.write(slaveRegister);
    Wire.endTransmission();
    delayMicroseconds(70);

    sendReply(sizeof(i2cReply), I2C_RESP_READ, i2cReply);
#endif
    }

static void handleWrite(int size, byte *msg)
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
    }
