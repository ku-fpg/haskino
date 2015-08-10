#include <Arduino.h>
#include "AmberComm.h"
#include "AmberCommands.h"
#include "AmberDigital.h"

static void handleReadPin(int size, byte *msg);
static void handleWritePin(int size, byte *msg);

void parseDigitalMessage(int size, byte *msg)
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
    }

static void handleReadPin(int size, byte *msg)
    {
    byte pinNo = msg[1];
    byte digitalReply = digitalRead(pinNo);

    sendReply(sizeof(digitalReply), DIG_RESP_READ_PIN, &digitalReply);
    }

static void handleWritePin(int size, byte *msg)
    {
    byte pinNo = msg[1];
    byte value = msg[2];

    digitalWrite(pinNo, value);
    }
