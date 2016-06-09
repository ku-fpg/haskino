/******************************************************************************
 *
 * Module      :  HaskinoRuntimeStepper
 * Copyright   :  (c) University of Kansas
 * License     :  BSD3
 * Stability   :  experimental
 *
 * Haskino Runtime Stepper module
 *****************************************************************************/

#include <Arduino.h>
#include <Stepper.h>
#include "HaskinoRuntime.h"

// Stepper routines

static Stepper *steppers[MAX_FIRM_STEPPERS];

static int nextStepper = 0;

uint8_t stepper2Pin(uint16_t steps, uint8_t p1, uint8_t p2)
    {
    Stepper *newStepper;
    uint8_t stepperIndex;

    newStepper = new Stepper(steps, p1, p2);
    stepperIndex = nextStepper;

    steppers[nextStepper++] = newStepper;
    return stepperIndex;
    }
    
uint8_t stepper4Pin(uint16_t steps, uint8_t p1, uint8_t p2, 
                    uint8_t p3, uint8_t p4)
    {
    Stepper *newStepper;
    uint8_t stepperIndex;

    newStepper = new Stepper(steps, p1, p2, p3, p4);
    stepperIndex = nextStepper;

    steppers[nextStepper++] = newStepper;
    return stepperIndex;
    }
    
void stepperSetSpeed(uint8_t st, int32_t sp)
    {
    steppers[st]->setSpeed(sp);
    }
    
