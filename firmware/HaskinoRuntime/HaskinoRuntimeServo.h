/******************************************************************************
 *
 * Module      :  HaskinoRuntimeServo
 * Copyright   :  (c) University of Kansas
 * License     :  BSD3
 * Stability   :  experimental
 *
 * Haskino Runtime Servo header
 *****************************************************************************/

#ifndef HaskinoRuntimeServoH
#define HaskinoRuntimeServoH

// Servo routines

uint8_t servoAttach(uint8_t p);
uint8_t servoAttachMinMax(uint8_t p, uint16_t min, uint16_t max);
void servoDetach(uint8_t sv);
void servoWrite(uint8_t sv, uint16_t deg);
void servoWriteMicros(uint8_t sv, uint16_t m);
uint16_t servoRead(uint8_t sv);
uint16_t servoReadMicros(uint8_t sv);

#endif /* HaskinoRuntimeServoH */
