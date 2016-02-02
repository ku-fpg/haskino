#include <Arduino.h>
#include <Servo.h>
#include "HaskinoServo.h"
#include "HaskinoCommands.h"
#include "HaskinoComm.h"
#include "HaskinoConfig.h"
#include "HaskinoExpr.h"

#ifdef INCLUDE_SRVO_CMDS
static Servo *servos[MAX_FIRM_SERVOS];

static int nextServo = 0;

static bool handleAttach(int size, const byte *msg, CONTEXT *context);
static bool handleDetach(int size, const byte *msg, CONTEXT *context);
static bool handleWrite(int size, const byte *msg, CONTEXT *context);
static bool handleWriteMicros(int size, const byte *msg, CONTEXT *context);
static bool handleRead(int size, const byte *msg, CONTEXT *context);
static bool handleReadMicros(int size, const byte *msg, CONTEXT *context);

bool parseServoMessage(int size, const byte *msg, CONTEXT *context)
    {
    switch (msg[0]) 
        {
        case SRVO_CMD_ATTACH:
            handleAttach(size, msg, context);
            break;
        case SRVO_CMD_DETACH:
            handleDetach(size, msg, context);
            break;
        case SRVO_CMD_WRITE:
            handleWrite(size, msg, context);
            break;
        case SRVO_CMD_WRITE_MICROS:
            handleWriteMicros(size, msg, context);
            break;
        case SRVO_CMD_READ:
            handleRead(size, msg, context);
            break;
        case SRVO_CMD_READ_MICROS:
            handleReadMicros(size, msg, context);
            break;
        }
    return false;
    }

static bool handleAttach(int size, const byte *msg, CONTEXT *context)
    {
    Servo *newServo;
    byte bind = msg[1];
    byte *expr = (byte *) &msg[2];
    byte pin = evalWord8Expr(&expr, context);
    int min = evalInt16Expr(&expr, context);
    int max = evalInt16Expr(&expr, context);
    byte servoReply[2];

    newServo = new Servo();
    newServo->attach(pin, min, max);
    servoReply[0] = EXPR(EXPR_BOOL, EXPR_LIT);
    servoReply[1] = nextServo;

    servos[nextServo++] = newServo;
    sendReply(sizeof(servoReply), SRVO_RESP_ATTACH, 
              servoReply, context, bind);
    return false;
    }

static bool handleDetach(int size, const byte *msg, CONTEXT *context)
    {
    byte *expr = (byte *) &msg[1];
    byte servoId = evalWord8Expr(&expr, context);

    servos[servoId]->detach();
    return false;
    }

static bool handleWrite(int size, const byte *msg, CONTEXT *context)
    {
    byte *expr = (byte *) &msg[1];
    byte servoId = evalWord8Expr(&expr, context);
    int deg = evalInt16Expr(&expr, context);

    servos[servoId]->write(deg);
    return false;
    }

static bool handleWriteMicros(int size, const byte *msg, CONTEXT *context)
    {
    byte *expr = (byte *) &msg[1];
    byte servoId = evalWord8Expr(&expr, context);
    int micros = evalInt16Expr(&expr, context);

    servos[servoId]->writeMicroseconds(micros);
    return false;
    }

static bool handleRead(int size, const byte *msg, CONTEXT *context)
    {
    byte bind = msg[1];
    byte *expr = (byte *) &msg[2];
    byte servoId = evalWord8Expr(&expr, context);
    uint16_t degValue;
    byte readReply[3];

    readReply[0] = EXPR(EXPR_WORD16, EXPR_LIT);
    degValue = servos[servoId]->read();
    memcpy(&readReply[1], &degValue, sizeof(degValue));

    sendReply(sizeof(readReply), SRVO_RESP_READ, 
              (byte *) &readReply, context, bind);
    return false;
    }

static bool handleReadMicros(int size, const byte *msg, CONTEXT *context)
    {
    byte bind = msg[1];
    byte *expr = (byte *) &msg[2];
    byte servoId = evalWord8Expr(&expr, context);
    uint16_t degValue;
    byte readReply[3];

    readReply[0] = EXPR(EXPR_WORD16, EXPR_LIT);
    degValue = servos[servoId]->readMicroseconds();
    memcpy(&readReply[1], &degValue, sizeof(degValue));

    sendReply(sizeof(readReply), SRVO_RESP_READ_MICROS, 
              (byte *) &readReply, context, bind);
    return false;
    }
#endif
