#include <Arduino.h>
#include "AmberAnalog.h"
#include "AmberComm.h"
#include "AmberCommands.h"

static void handleReadPin(int size, byte *msg);
static void handleWritePin(int size, byte *msg);

void parseAnalogMessage(int size, byte *msg)
    {
    switch (msg[0] ) 
        {
        case ALG_CMD_READ_PIN:
            handleReadPin(size, msg);
            break;
        case ALG_CMD_WRITE_PIN:
            handleWritePin(size, msg);
            break;
        }
    }

static void handleReadPin(int size, byte *msg)
    {
    byte pinNo = msg[1];
    uint16_t analogReply;

    analogReply = analogRead(pinNo);
    sendReply(sizeof(analogReply), ALG_RESP_READ_PIN, (byte *) &analogReply);
    }

static void handleWritePin(int size, byte *msg)
    {
    byte pinNo = msg[1];
    byte value = msg[2];

    analogWrite(pinNo, value);
    }

