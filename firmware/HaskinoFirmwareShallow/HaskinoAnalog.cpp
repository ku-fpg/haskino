#include <Arduino.h>
#include "HaskinoAnalog.h"
#include "HaskinoComm.h"
#include "HaskinoCommands.h"
#include "HaskinoConfig.h"

#ifdef INCLUDE_ALG_CMDS
static void handleReadPin(int size, const byte *msg);
static void handleWritePin(int size, const byte *msg);
static void handleTonePin(int size, const byte *msg);
static void handleNoTonePin(int size, const byte *msg);

void parseAnalogMessage(int size, const byte *msg)
    {
    switch (msg[0] ) 
        {
        case ALG_CMD_READ_PIN:
            return handleReadPin(size, msg);
            break;
        case ALG_CMD_WRITE_PIN:
            return handleWritePin(size, msg);
            break;
        case ALG_CMD_TONE_PIN:
            return handleTonePin(size, msg);
            break;
        case ALG_CMD_NOTONE_PIN:
            return handleNoTonePin(size, msg);
            break;
        }
    }

static void handleReadPin(int size, const byte *msg)
    {    
    byte pinNo;
    uint16_t analogValue;
    byte analogReply[4];

    if ( msg[2] == EXPR_WORD8 && msg[3] == EXPR_LIT)
        {
        pinNo = msg[4];
        analogReply[0] = EXPR_WORD16;
        analogReply[1] = EXPR_LIT;
        analogValue = analogRead(pinNo);
        memcpy(&analogReply[2], &analogValue, sizeof(analogValue));

        sendReply(sizeof(analogReply), ALG_RESP_READ_PIN, 
                  (byte *) &analogReply);
        }
    }

static void handleWritePin(int size, const byte *msg)
    {
    byte pinNo;
    byte value;

    if ( msg[1] == EXPR_WORD8 && msg[2] == EXPR_LIT &&
         msg[4] == EXPR_WORD8 && msg[5] == EXPR_LIT )
        {
        pinNo = msg[3];
        value = msg[6];

        analogWrite(pinNo, value);
        }
    }

static void handleTonePin(int size, const byte *msg)
    {
    byte pinNo;
    unsigned int freq;
    unsigned long duration;

    if ( msg[1] == EXPR_WORD8  && msg[2] == EXPR_LIT &&
         msg[4] == EXPR_WORD16 && msg[5] == EXPR_LIT &&
         msg[8] == EXPR_WORD32 && msg[9] == EXPR_LIT )
        {
        pinNo = msg[3];
        duration = ((uint32_t) msg[11] << 24) | ((uint32_t) msg[10] << 16) | 
                   ((uint32_t) msg[9] << 8) | (uint32_t) msg[8];
        freq = ((uint16_t) msg[6] << 8) | (uint16_t) msg[5];

        if (duration == 0)
            {
            tone(pinNo, freq);
            }
        else
            {
            tone(pinNo, freq, duration);
            }
        }
    }

static void handleNoTonePin(int size, const byte *msg)
    {
    byte pinNo;

    if ( msg[2] == EXPR_WORD8 && msg[3] == EXPR_LIT)
        {
        pinNo = msg[4];
        noTone(pinNo);
        }
    }
#endif

