#include <Arduino.h>
#include "HaskinoAnalog.h"
#include "HaskinoComm.h"
#include "HaskinoCommands.h"
#include "HaskinoConfig.h"
#include "HaskinoExpr.h"

#ifdef INCLUDE_ALG_CMDS
static bool handleReadPin(int size, const byte *msg, CONTEXT *context);
static bool handleWritePin(int size, const byte *msg, CONTEXT *context);
static bool handleTonePin(int size, const byte *msg, CONTEXT *context);
static bool handleNoTonePin(int size, const byte *msg, CONTEXT *context);

bool parseAnalogMessage(int size, const byte *msg, CONTEXT *context)
    {
    switch (msg[0] ) 
        {
        case ALG_CMD_READ_PIN:
            return handleReadPin(size, msg, context);
            break;
        case ALG_CMD_WRITE_PIN:
            return handleWritePin(size, msg, context);
            break;
        case ALG_CMD_TONE_PIN:
            return handleTonePin(size, msg, context);
            break;
        case ALG_CMD_NOTONE_PIN:
            return handleNoTonePin(size, msg, context);
            break;
        }
    return false;
    }

static bool handleReadPin(int size, const byte *msg, CONTEXT *context)
    {
    byte bind = msg[1];
    byte *expr = (byte *) &msg[2];
    byte pinNo = evalWord8Expr(&expr, context);
    uint16_t analogValue;
    byte analogReply[3];

    analogReply[0] = EXPR(EXPR_WORD16, EXPR_LIT);
    analogValue = analogRead(pinNo);
    memcpy(&analogReply[1], &analogValue, sizeof(analogValue));

    sendReply(sizeof(analogReply), ALG_RESP_READ_PIN, 
              (byte *) &analogReply, context, bind);
    return false;
    }

static bool handleWritePin(int size, const byte *msg, CONTEXT *context)
    {
    byte *expr = (byte *) &msg[1];
    byte pinNo = evalWord8Expr(&expr, context);
    byte value = evalWord8Expr(&expr, context);

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

static bool handleTonePin(int size, const byte *msg, CONTEXT *context)
    {
    byte *expr = (byte *) &msg[1];
    byte pinNo = evalWord8Expr(&expr, context);
    unsigned int freq = evalWord16Expr(&expr, context);
    unsigned long duration = evalWord32Expr(&expr, context);

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

static bool handleNoTonePin(int size, const byte *msg, CONTEXT *context)
    {
    byte *expr = (byte *) &msg[1];
    byte pinNo = evalWord8Expr(&expr, context);

    noTone(pinNo);
    return false;
    }
#endif

