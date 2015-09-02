#include <Arduino.h>
#include "HaskinoAnalog.h"
#include "HaskinoComm.h"
#include "HaskinoCommands.h"
#include "HaskinoExpr.h"

static bool handleReadPin(int size, const byte *msg, byte *local);
static bool handleWritePin(int size, const byte *msg);
static bool handleTonePin(int size, const byte *msg);
static bool handleNoTonePin(int size, const byte *msg);
static bool handleReadPinE(int size, const byte *msg, byte *local);
static bool handleWritePinE(int size, const byte *msg);
static bool handleTonePinE(int size, const byte *msg);
static bool handleNoTonePinE(int size, const byte *msg);

bool parseAnalogMessage(int size, const byte *msg, byte *local)
    {
    switch (msg[0] ) 
        {
        case ALG_CMD_READ_PIN:
            return handleReadPin(size, msg, local);
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
        case ALG_CMD_READ_PIN_E:
            return handleReadPinE(size, msg, local);
            break;
        case ALG_CMD_WRITE_PIN_E:
            return handleWritePinE(size, msg);
            break;
        case ALG_CMD_TONE_PIN_E:
            return handleTonePinE(size, msg);
            break;
        case ALG_CMD_NOTONE_PIN_E:
            return handleNoTonePinE(size, msg);
            break;
        }
    return false;
    }

static bool handleReadPin(int size, const byte *msg, byte *local)
    {
    byte pinNo = msg[1];
    uint16_t analogReply;

    analogReply = analogRead(pinNo);
    sendReply(sizeof(analogReply), ALG_RESP_READ_PIN, 
              (byte *) &analogReply, local);
    return false;
    }

static bool handleWritePin(int size, const byte *msg)
    {
    byte pinNo = msg[1];
    byte value = msg[2];

    analogWrite(pinNo, value);
    return false;
    }

static bool handleTonePin(int size, const byte *msg)
    {
    byte pinNo = msg[1];
    unsigned int freq;
    memcpy(&freq, &msg[2], sizeof(unsigned int));
    unsigned long duration;
    memcpy(&duration, &msg[4], sizeof(unsigned long));

    if (duration == 0)
        {
        tone(pinNo, freq);
        }
    else
        {
        tone(pinNo, freq, duration);
        }
    return false;
    }

static bool handleNoTonePin(int size, const byte *msg)
    {
    byte pinNo = msg[1];

    noTone(pinNo);
    return false;
    }

static bool handleReadPinE(int size, const byte *msg, byte *local)
    {
    byte *expr = (byte *) &msg[1];
    byte pinNo = evalWord8Expr(&expr);
    uint16_t analogReply;

    analogReply = analogRead(pinNo);
    sendReply(sizeof(analogReply), ALG_RESP_READ_PIN, 
              (byte *) &analogReply, local);
    return false;
    }

static bool handleWritePinE(int size, const byte *msg)
    {
    byte *expr = (byte *) &msg[1];
    byte pinNo = evalWord8Expr(&expr);
    byte value = evalWord8Expr(&expr);

    analogWrite(pinNo, value);
    return false;
    }

static bool handleTonePinE(int size, const byte *msg)
    {
    byte *expr = (byte *) &msg[1];
    byte pinNo = evalWord8Expr(&expr);
    unsigned int freq = evalWord16Expr(&expr);
    unsigned long duration = evalWord32Expr(&expr);

    if (duration == 0)
        {
        tone(pinNo, freq);
        }
    else
        {
        tone(pinNo, freq, duration);
        }
    return false;
    }

static bool handleNoTonePinE(int size, const byte *msg)
    {
    byte *expr = (byte *) &msg[1];
    byte pinNo = evalWord8Expr(&expr);

    noTone(pinNo);
    return false;
    }


