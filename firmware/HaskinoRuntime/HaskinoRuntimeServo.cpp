/******************************************************************************
 *
 * Module      :  HaskinoRuntimeServo
 * Copyright   :  (c) University of Kansas
 * License     :  BSD3
 * Stability   :  experimental
 *
 * Haskino Runtime Servo module
 *****************************************************************************/

#include <Arduino.h>
#include <Servo.h>
#include "HaskinoRuntime.h"

// Servo routines
#define DEFAULT_SERVO_MIN 544
#define DEFAULT_SERVO_MAX 2400

static Servo *servos[MAX_FIRM_SERVOS];

static int nextServo = 0;

uint8_t servoAttach(uint8_t pin)
    {
    return servoAttachMinMax(pin, DEFAULT_SERVO_MIN, DEFAULT_SERVO_MAX);
    }
    
uint8_t servoAttachMinMax(uint8_t pin, uint16_t min, uint16_t max)
    {
    Servo *newServo;
    uint8_t servoIndex;

    newServo = new Servo();
    newServo->attach(pin, min, max);
    servoIndex = nextServo;

    servos[nextServo++] = newServo;

    return servoIndex;
    }
    
void servoDetach(uint8_t sv)
    {
    servos[sv]->detach();
    }
    
void servoWrite(uint8_t sv, uint16_t deg)
    {
    servos[sv]->write(deg);
    }
    
void servoWriteMicros(uint8_t sv, uint16_t micros)
    {
    servos[sv]->writeMicroseconds(micros);
    }
    
uint16_t servoRead(uint8_t sv)
    {
    return servos[sv]->read();
    }
    
uint16_t servoReadMicros(uint8_t sv)
    {
    return servos[sv]->readMicroseconds();
    }
    
