#include <Arduino.h>
#include "HaskinoComm.h"
#include "HaskinoCommands.h"
#include "HaskinoConfig.h"
#include "HaskinoDigital.h"

#ifdef INCLUDE_DIG_CMDS
static void handleReadPin(int size, const byte *msg);
static void handleWritePin(int size, const byte *msg);
static void handleReadPort(int size, const byte *msg);
static void handleWritePort(int size, const byte *msg);

void parseDigitalMessage(int size, const byte *msg)
    {
    switch (msg[0])
        {
        case DIG_CMD_READ_PIN:
            handleReadPin(size, msg);
            break;
        case DIG_CMD_WRITE_PIN:
            handleWritePin(size, msg);
            break;
        case DIG_CMD_READ_PORT:
            handleReadPort(size, msg);
            break;
        case DIG_CMD_WRITE_PORT:
            handleWritePort(size, msg);
            break;
        }
    }

static void handleReadPin(int size, const byte *msg)
    {
    byte pinNo;
    byte digitalReply[3];

    if ( msg[2] == EXPR_WORD8 && msg[3] == EXPR_LIT)
        {
        pinNo = msg[4];
        digitalReply[0] = EXPR_BOOL;
        digitalReply[1] = EXPR_LIT;
        digitalReply[2] = digitalRead(pinNo);

        sendReply(sizeof(digitalReply), DIG_RESP_READ_PIN, digitalReply);
        }
    }

static void handleWritePin(int size, const byte *msg)
    {
    byte pinNo;
    byte value;

    if ( msg[1] == EXPR_WORD8  && msg[2] == EXPR_LIT &&
         msg[4] == EXPR_BOOL   && msg[5] == EXPR_LIT )
        {
        pinNo = msg[3];
        value = msg[6];

        digitalWrite(pinNo, value);
        }
    }

static uint8_t bits[8] = {0x01,0x02,0x04,0x08,0x10,0x20,0x40,0x80};

static void handleReadPort(int size, const byte *msg)
    {
    byte pinNo;
    byte mask;
    byte digitalReply[3];

    if ( msg[1] == EXPR_WORD8  && msg[2] == EXPR_LIT &&
         msg[4] == EXPR_WORD8  && msg[5] == EXPR_LIT )
        {
        pinNo = msg[3];
        mask = msg[6];

        digitalReply[0] = EXPR_WORD8;
        digitalReply[1] = EXPR_LIT;
        digitalReply[2] = 0;
        for (int i=0;i<8;i++)
            {
            if ((bits[i] & mask) && digitalRead(pinNo+i))
                digitalReply[1] |= bits[i];
            }

        sendReply(sizeof(digitalReply), DIG_RESP_READ_PORT, digitalReply);
        }
    }

static void handleWritePort(int size, const byte *msg)
    {
    byte pinNo;
    byte value;
    byte mask;

    if ( msg[1] == EXPR_WORD8  && msg[2] == EXPR_LIT &&
         msg[4] == EXPR_WORD8  && msg[5] == EXPR_LIT &&
         msg[7] == EXPR_WORD8  && msg[8] == EXPR_LIT )
        {
        pinNo = msg[3];
        value = msg[6];
        mask = msg[9];

        for (int i=0;i<8;i++)
            {
            if (bits[i] & mask)
                {
                digitalWrite(pinNo+i, (bits[i] & value) == bits[i]);
                }
            }
        }
    }
#endif
