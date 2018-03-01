#include <Arduino.h>
#include <Servo.h>
#include "HaskinoServo.h"
#include "HaskinoCommands.h"
#include "HaskinoComm.h"
#include "HaskinoConfig.h"

#ifdef INCLUDE_SRVO_CMDS
static Servo *servos[MAX_FIRM_SERVOS];

static int nextServo = 0;

static void handleAttach(int size, const byte *msg);
static void handleDetach(int size, const byte *msg);
static void handleWrite(int size, const byte *msg);
static void handleWriteMicros(int size, const byte *msg);
static void handleRead(int size, const byte *msg);
static void handleReadMicros(int size, const byte *msg);

void parseServoMessage(int size, const byte *msg)
    {
    switch (msg[0]) 
        {
        case SRVO_CMD_ATTACH:
            handleAttach(size, msg);
            break;
        case SRVO_CMD_DETACH:
            handleDetach(size, msg);
            break;
        case SRVO_CMD_WRITE:
            handleWrite(size, msg);
            break;
        case SRVO_CMD_WRITE_MICROS:
            handleWriteMicros(size, msg);
            break;
        case SRVO_CMD_READ:
            handleRead(size, msg);
            break;
        case SRVO_CMD_READ_MICROS:
            handleReadMicros(size, msg);
            break;
        }
    }

static void handleAttach(int size, const byte *msg)
    {
    Servo *newServo;
    byte pin;
    byte servoId;
    int min;
    int max;
    byte servoReply[3];

    if ( msg[2] == EXPR_WORD8  && msg[3] == EXPR_LIT &&
         msg[5] == EXPR_WORD16 && msg[6] == EXPR_LIT &&
         msg[9] == EXPR_WORD16 && msg[10] == EXPR_LIT )
        {
        pin = msg[4];
        min = ((int16_t) msg[8] << 8) | (uint16_t) msg[7];
        max = ((int16_t) msg[12] << 8) | (uint16_t) msg[11];

        newServo = new Servo();
        newServo->attach(pin, min, max);
        servoReply[0] = EXPR_BOOL;
        servoReply[1] = EXPR_LIT;
        servoReply[2] = nextServo;

        servos[nextServo++] = newServo;
        sendReply(sizeof(servoReply), SRVO_RESP_ATTACH, servoReply);
        }
    }

static void handleDetach(int size, const byte *msg)
    {
    byte servoId;

    if ( msg[1] == EXPR_WORD8 && msg[2] == EXPR_LIT)
        {
        servoId = msg[3];

        servos[servoId]->detach();
        }   
    }

static void handleWrite(int size, const byte *msg)
    {
    byte servoId;
    int deg;

    if ( msg[1] == EXPR_WORD8  && msg[2] == EXPR_LIT &&
         msg[4] == EXPR_WORD16 && msg[5] == EXPR_LIT ) 
        {
        servoId = msg[3];
        deg = ((int16_t) msg[7] << 8) | (uint16_t) msg[6];

        servos[servoId]->write(deg);
        }
    }

static void handleWriteMicros(int size, const byte *msg)
    {
    byte servoId;
    int micros;

    if ( msg[1] == EXPR_WORD8  && msg[2] == EXPR_LIT &&
         msg[4] == EXPR_WORD16 && msg[5] == EXPR_LIT ) 
        {
        servoId = msg[3];
        micros = ((int16_t) msg[7] << 8) | (uint16_t) msg[6];

        servos[servoId]->writeMicroseconds(micros);
        }
    }

static void handleRead(int size, const byte *msg)
    {
    byte servoId;
    uint16_t degValue;
    byte readReply[4];

    if ( msg[2] == EXPR_WORD8 && msg[3] == EXPR_LIT )
        {
        servoId = msg[4];

        readReply[0] = EXPR_WORD16;
        readReply[1] = EXPR_LIT;
        degValue = servos[servoId]->read();
        memcpy(&readReply[2], &degValue, sizeof(degValue));

        sendReply(sizeof(readReply), SRVO_RESP_READ, (byte *) &readReply);
        }
    }

static void handleReadMicros(int size, const byte *msg)
    {
    byte servoId;
    uint16_t degValue;
    byte readReply[4];

    if ( msg[2] == EXPR_WORD8 && msg[3] == EXPR_LIT )
        {
        servoId = msg[4];

        readReply[0] = EXPR_WORD16;
        readReply[1] = EXPR_LIT;
        degValue = servos[servoId]->readMicroseconds();
        memcpy(&readReply[2], &degValue, sizeof(degValue));

        sendReply(sizeof(readReply), SRVO_RESP_READ_MICROS, (byte *) &readReply);
        }
    }
#endif
