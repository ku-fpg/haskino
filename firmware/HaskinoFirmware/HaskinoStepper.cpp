#include <Arduino.h>
#include <Stepper.h>
#include "HaskinoStepper.h"
#include "HaskinoCommands.h"
#include "HaskinoComm.h"
#include "HaskinoConfig.h"
#include "HaskinoExpr.h"

#ifdef INCLUDE_STEP_CMDS
static Stepper *steppers[MAX_FIRM_STEPPERS];

static int nextStepper = 0;

static bool handle2Pin(int size, const byte *msg, CONTEXT *context);
static bool handle4Pin(int size, const byte *msg, CONTEXT *context);
static bool handleSetSpeed(int size, const byte *msg, CONTEXT *context);
static bool handleStep(int size, const byte *msg, CONTEXT *context);

bool parseStepperMessage(int size, const byte *msg, CONTEXT *context)
    {
    switch (msg[0]) 
        {
        case STEP_CMD_2PIN:
            handle2Pin(size, msg, context);
            break;
        case STEP_CMD_4PIN:
            handle4Pin(size, msg, context);
            break;
        case STEP_CMD_SET_SPEED:
            handleSetSpeed(size, msg, context);
            break;
        case STEP_CMD_STEP:
            handleStep(size, msg, context);
            break;
        }
    return false;
    }

static bool handle2Pin(int size, const byte *msg, CONTEXT *context)
    {
    Stepper *newStepper;
    byte bind = msg[1];
    byte *expr = (byte *) &msg[2];
    uint16_t steps = evalWord16Expr(&expr, context);
    byte pin1No = evalWord8Expr(&expr, context);
    byte pin2No = evalWord8Expr(&expr, context);
    byte stepperReply[2];

    newStepper = new Stepper(steps, pin1No, pin2No);
    stepperReply[0] = EXPR(EXPR_BOOL, EXPR_LIT);
    stepperReply[1] = nextStepper;

    steppers[nextStepper++] = newStepper;
    sendReply(sizeof(stepperReply), STEP_RESP_2PIN, 
              stepperReply, context, bind);
    return false;
    }

static bool handle4Pin(int size, const byte *msg, CONTEXT *context)
    {
    Stepper *newStepper;
    byte bind = msg[1];
    byte *expr = (byte *) &msg[2];
    uint16_t steps = evalWord16Expr(&expr, context);
    byte pin1No = evalWord8Expr(&expr, context);
    byte pin2No = evalWord8Expr(&expr, context);
    byte pin3No = evalWord8Expr(&expr, context);
    byte pin4No = evalWord8Expr(&expr, context);
    byte stepperReply[2];

    newStepper = new Stepper(steps, pin1No, pin2No, pin3No, pin4No);
    stepperReply[0] = EXPR(EXPR_BOOL, EXPR_LIT);
    stepperReply[1] = nextStepper;

    steppers[nextStepper++] = newStepper;
    sendReply(sizeof(stepperReply), STEP_RESP_4PIN, 
              stepperReply, context, bind);
    return false;
    }

static bool handleSetSpeed(int size, const byte *msg, CONTEXT *context)
    {
    byte *expr = (byte *) &msg[1];
    byte stepperId = evalWord8Expr(&expr, context);
    uint32_t speed = evalWord32Expr(&expr, context);

    steppers[stepperId]->setSpeed(speed);
    return false;
    }

static bool handleStep(int size, const byte *msg, CONTEXT *context)
    {
    byte bind = msg[1];
    byte *expr = (byte *) &msg[2];
    byte stepperId = evalWord8Expr(&expr, context);
    int16_t steps = evalInt16Expr(&expr, context);

    steppers[stepperId]->step(steps);
    if (context->currBlockLevel <= 0)
        {
        sendReply(0, STEP_RESP_STEP, NULL, context, bind);
        }
    return false;
    }
#endif
