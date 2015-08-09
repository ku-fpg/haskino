#include <Arduino.h>
#include "AmberComm.h"
#include "AmberCommands.h"
#include "AmberDigital.h"

static void handleReadPin(int size, unsigned char *msg);
static void handleWritePin(int size, unsigned char *msg);

void parseDigitalMessage(int size, unsigned char *msg)
    {
    switch (msg[0] ) 
        {
        case DIG_CMD_READ_PIN:
            handleReadPin(size, msg);
            break;
        case DIG_CMD_WRITE_PIN:
            handleWritePin(size, msg);
            break;
        }
    }

static void handleReadPin(int size, unsigned char *msg)
    {
    unsigned char digitalReply[2];

    digitalReply[0] = DIG_RESP_READ_PIN;
    digitalReply[1] = digitalRead(msg[1]);
    sendReply(sizeof(digitalReply), digitalReply);
    }

static void handleWritePin(int size, unsigned char *msg)
    {
    digitalWrite(msg[1],msg[2]);
    }

