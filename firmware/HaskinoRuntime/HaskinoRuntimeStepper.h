/******************************************************************************
 *
 * Module      :  HaskinoRuntimeStepper
 * Copyright   :  (c) University of Kansas
 * License     :  BSD3
 * Stability   :  experimental
 *
 * Haskino Runtime Stepper header
 *****************************************************************************/

#ifndef HaskinoRuntimeStepperH
#define HaskinoRuntimeStepperH

// Stepper routines

uint8_t stepper2Pin(uint16_t s, uint8_t p1, uint8_t p2);
uint8_t stepper4Pin(uint16_t s, uint8_t p1, uint8_t p2, uint8_t p3, uint8_t p4);
void stepperSetSpeed(uint8_t st, int32_t sp);

#endif /* HaskinoRuntimeStepperH */

