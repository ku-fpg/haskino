#include <Arduino.h>
#include <Stepper.h>
#include "HaskinoStepper.h"
#include "HaskinoCommands.h"
#include "HaskinoComm.h"
#include "HaskinoConfig.h"

#ifdef INCLUDE_STEP_CMDS
static Stepper *steppers[MAX_FIRM_STEPPERS];

static int nextStepper = 0;

static void handle2Pin(int size, const byte *msg);
static void handle4Pin(int size, const byte *msg);
static void handleSetSpeed(int size, const byte *msg);
static void handleStep(int size, const byte *msg);

void parseStepperMessage(int size, const byte *msg)
    {
    switch (msg[0]) 
        {
        case STEP_CMD_2PIN:
            handle2Pin(size, msg);
            break;
        case STEP_CMD_4PIN:
            handle4Pin(size, msg);
            break;
        case STEP_CMD_SET_SPEED:
            handleSetSpeed(size, msg);
            break;
        case STEP_CMD_STEP:
            handleStep(size, msg);
            break;
        }
    }

static void handle2Pin(int size, const byte *msg)
    {
    Stepper *newStepper;
    uint16_t steps;
    byte pin1No;
    byte pin2No;
    byte stepperReply[3];

    if ( msg[2] == EXPR_WORD16 && msg[3] == EXPR_LIT &&
         msg[6] == EXPR_WORD8  && msg[7] == EXPR_LIT &&
         msg[9] == EXPR_WORD8  && msg[10] == EXPR_LIT )
        {
        steps = ((uint16_t) msg[5] << 8) | (uint32_t) msg[4];
        pin1No = msg[8];
        pin2No = msg[11];

        newStepper = new Stepper(steps, pin1No, pin2No);
        stepperReply[0] = EXPR_WORD8;
        stepperReply[1] = EXPR_LIT;
        stepperReply[2] = nextStepper;

        steppers[nextStepper++] = newStepper;
        sendReply(sizeof(stepperReply), STEP_RESP_2PIN, stepperReply);
        }
    }

static void handle4Pin(int size, const byte *msg)
    {
    Stepper *newStepper;
    uint16_t steps;
    byte pin1No;
    byte pin2No;
    byte pin3No;
    byte pin4No;
    byte stepperReply[3];

    if ( msg[2] == EXPR_WORD16 && msg[3] == EXPR_LIT &&
         msg[6] == EXPR_WORD8  && msg[7] == EXPR_LIT &&
         msg[9] == EXPR_WORD8  && msg[10] == EXPR_LIT &&
         msg[12] == EXPR_WORD8 && msg[13] == EXPR_LIT &&
         msg[15] == EXPR_WORD8 && msg[16] == EXPR_LIT )
        {
        steps = ((uint16_t) msg[5] << 8) | (uint32_t) msg[4];
        pin1No = msg[8];
        pin2No = msg[11];
        pin3No = msg[14];
        pin4No = msg[17];

        newStepper = new Stepper(steps, pin1No, pin2No, pin3No, pin4No);
        stepperReply[0] = EXPR_WORD8;
        stepperReply[1] = EXPR_LIT;
        stepperReply[2] = nextStepper;

        steppers[nextStepper++] = newStepper;
        sendReply(sizeof(stepperReply), STEP_RESP_4PIN, stepperReply);
        }
    }

static void handleSetSpeed(int size, const byte *msg)
    {
    byte stepperId;
    uint32_t speed;

    if ( msg[1] == EXPR_WORD8  && msg[2] == EXPR_LIT &&
         msg[4] == EXPR_WORD32 && msg[5] == EXPR_LIT )
        {
        stepperId = msg[3];
        speed = ((uint32_t) msg[9] << 24) | ((uint32_t) msg[8] << 16) | 
                 ((uint32_t) msg[7] << 8) | (uint32_t) msg[6];
        
        steppers[stepperId]->setSpeed(speed);
        }
    }

static void handleStep(int size, const byte *msg)
    {
    byte stepperId;
    int16_t steps;

    if ( msg[1] == EXPR_WORD8  && msg[2] == EXPR_LIT &&
         msg[4] == EXPR_WORD16 && msg[5] == EXPR_LIT )
        {
        stepperId = msg[3];
        steps = ((uint16_t) msg[7] << 8) | (uint16_t) msg[6];
    
        steppers[stepperId]->step(steps);
        sendReply(0, STEP_RESP_STEP, NULL);
        }
    }
#endif
