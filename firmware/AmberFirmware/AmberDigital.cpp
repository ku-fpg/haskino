#include <Arduino.h>
#include "AmberComm.h"
#include "AmberCommands.h"
#include "AmberDigital.h"

static int handleReadPin(int size, byte *msg);
static int handleWritePin(int size, byte *msg);

int parseDigitalMessage(int size, byte *msg)
    {
    switch (msg[0]) 
        {
        case DIG_CMD_READ_PIN:
            return handleReadPin(size, msg);
            break;
        case DIG_CMD_WRITE_PIN:
            return handleWritePin(size, msg);
            break;
        }
    }

static int handleReadPin(int size, byte *msg)
    {
    byte pinNo = msg[1];
    byte digitalReply = digitalRead(pinNo);

    sendReply(sizeof(digitalReply), DIG_RESP_READ_PIN, &digitalReply);
    return 2;
    }

static int handleWritePin(int size, byte *msg)
    {
    byte pinNo = msg[1];
    byte value = msg[2];

    digitalWrite(pinNo, value);
    return 3;
    }
