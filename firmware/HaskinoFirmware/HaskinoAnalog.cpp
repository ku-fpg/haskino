#include <Arduino.h>
#include "HaskinoAnalog.h"
#include "HaskinoComm.h"
#include "HaskinoCommands.h"
#include "HaskinoExpr.h"

static bool handleReadPin(int size, const byte *msg, byte *local);
static bool handleWritePin(int size, const byte *msg, byte *local);
static bool handleTonePin(int size, const byte *msg, byte *local);
static bool handleNoTonePin(int size, const byte *msg, byte *local);

bool parseAnalogMessage(int size, const byte *msg, byte *local)
    {
    switch (msg[0] ) 
        {
        case ALG_CMD_READ_PIN:
            return handleReadPin(size, msg, local);
            break;
        case ALG_CMD_WRITE_PIN:
            return handleWritePin(size, msg, local);
            break;
        case ALG_CMD_TONE_PIN:
            return handleTonePin(size, msg, local);
            break;
        case ALG_CMD_NOTONE_PIN:
            return handleNoTonePin(size, msg, local);
            break;
        }
    return false;
    }

static bool handleReadPin(int size, const byte *msg, byte *local)
    {
    byte *expr = (byte *) &msg[1];
    byte pinNo = evalWord8ExprOrBind(&expr, local);
    uint16_t analogValue;
    byte analogReply[3];

    analogReply[0] = EXPR(EXPR_WORD16, EXPR_LIT);
    analogValue = analogRead(pinNo);
    memcpy(&analogReply[1], &analogValue, sizeof(analogValue));

    sendReply(sizeof(analogReply), ALG_RESP_READ_PIN, 
              (byte *) &analogReply, local);
    return false;
    }

static bool handleWritePin(int size, const byte *msg, byte *local)
    {
    byte *expr = (byte *) &msg[1];
    byte pinNo = evalWord8ExprOrBind(&expr, local);
    byte value = evalWord8ExprOrBind(&expr, local);

#ifdef INTEL_EDISON
    // ToDo:
    if (value == 0)
        {
        pwm_disable(pinNo);
        }
    else
        {
        pwm_configure(pinNo, 20408163, value * (20408163 / 255));
        pwm_enable(pinNo);
        }
#else
    analogWrite(pinNo, value);
#endif
    return false;
    }

static bool handleTonePin(int size, const byte *msg, byte *local)
    {
    byte *expr = (byte *) &msg[1];
    byte pinNo = evalWord8ExprOrBind(&expr, local);
    unsigned int freq = evalWord16ExprOrBind(&expr, local);
    unsigned long duration = evalWord32ExprOrBind(&expr, local);

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

static bool handleNoTonePin(int size, const byte *msg, byte *local)
    {
    byte *expr = (byte *) &msg[1];
    byte pinNo = evalWord8ExprOrBind(&expr, local);

    noTone(pinNo);
    return false;
    }


