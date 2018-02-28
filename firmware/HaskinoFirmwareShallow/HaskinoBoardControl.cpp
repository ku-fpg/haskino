#include <Arduino.h>
#include "HaskinoBoardControl.h"
#include "HaskinoComm.h"
#include "HaskinoCommands.h"
#include "HaskinoConfig.h"
#include "HaskinoFirmware.h"
#include "SoftReset.h"

static void handleSystemReset(int size, const byte *msg);
static void handleSetPinMode(int size, const byte *msg);
static void handleDelayMillis(int size, const byte *msg);
static void handleDelayMicros(int size, const byte *msg);

void parseBoardControlMessage(int size, const byte *msg)
    {
    switch (msg[0] ) 
        {
        case BC_CMD_SYSTEM_RESET:
            return handleSystemReset(size, msg);
            break;
        case BC_CMD_SET_PIN_MODE:
            return handleSetPinMode(size, msg);
            break;
        case BC_CMD_DELAY_MILLIS:
            return handleDelayMillis(size, msg);
            break;
        case BC_CMD_DELAY_MICROS:
            return handleDelayMicros(size, msg);
            break;
        }
    }

static void handleSetPinMode(int size, const byte *msg)
    {
    byte pinNo;
    byte value;

    if ( msg[1] == EXPR_WORD8 && msg[2] == EXPR_LIT &&
         msg[4] == EXPR_WORD8 && msg[5] == EXPR_LIT )
        {
        pinNo = msg[3];
        value = msg[6];

        pinMode(pinNo, value);
        }
    }

static void handleDelayMillis(int size, const byte *msg)
    {
    uint32_t millis;

    if ( msg[2] == EXPR_WORD32 && msg[3] == EXPR_LIT )
        {
        millis = ((uint32_t) msg[7] << 24) | ((uint32_t) msg[6] << 16) | 
                 ((uint32_t) msg[5] << 8) | (uint32_t) msg[4];

        delay(millis);
        sendReply(0, BC_RESP_DELAY, NULL);
        }
    }

static void handleDelayMicros(int size, const byte *msg)
    {
    uint32_t micros;

    if ( msg[2] == EXPR_WORD32 && msg[3] == EXPR_LIT )
        {
        micros = ((uint32_t) msg[7] << 24) | ((uint32_t) msg[6] << 16) | 
                 ((uint32_t) msg[5] << 8) | (uint32_t) msg[4];

        delayMicroseconds(micros);
        sendReply(0, BC_RESP_DELAY, NULL);
        }
    }

static void handleSystemReset(int size, const byte *msg)
    {
    soft_restart();
    }
