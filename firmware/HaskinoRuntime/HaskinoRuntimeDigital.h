/******************************************************************************
 *
 * Module      :  HaskinoRuntimeDigital
 * Copyright   :  (c) University of Kansas
 * License     :  BSD3
 * Stability   :  experimental
 *
 * Haskino Runtime Digital header
 *****************************************************************************/

#ifndef HaskinoRuntimeDigitalH
#define HaskinoRuntimeDigitalH

// Digital port routines

void digitalPortWrite(uint8_t p, uint8_t b, uint8_t m);
uint8_t digitalPortRead(uint8_t p, uint8_t m);

#endif /* HaskinoRuntimeDigitalH */
