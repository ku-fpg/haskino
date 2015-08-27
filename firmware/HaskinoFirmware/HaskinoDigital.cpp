#include <Arduino.h>
#include "AmberComm.h"
#include "AmberCommands.h"
#include "AmberDigital.h"

static bool handleReadPin(int size, byte *msg);
static bool handleWritePin(int size, byte *msg);

bool parseDigitalMessage(int size, byte *msg)
    {
    switch (msg[0]) 
        {
        case DIG_CMD_READ_PIN:
            handleReadPin(size, msg);
            break;
        case DIG_CMD_WRITE_PIN:
            handleWritePin(size, msg);
            break;
        }
    return false;
    }

static bool handleReadPin(int size, byte *msg)
    {
    byte pinNo = msg[1];
    byte digitalReply = digitalRead(pinNo);

    sendReply(sizeof(digitalReply), DIG_RESP_READ_PIN, &digitalReply);
    return false;
    }

static bool handleWritePin(int size, byte *msg)
    {
    byte pinNo = msg[1];
    byte value = msg[2];

    digitalWrite(pinNo, value);
    return false;
    }
