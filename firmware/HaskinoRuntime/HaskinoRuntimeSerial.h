/******************************************************************************
 *
 * Module      :  HaskinoRuntimeSerial
 * Copyright   :  (c) University of Kansas
 * License     :  BSD3
 * Stability   :  experimental
 *
 * Haskino Runtime Serial header
 *****************************************************************************/

#ifndef HaskinoRuntimeSerialH
#define HaskinoRuntimeSerialH

// Serial port routines

uint32_t serialAvailable(uint8_t p);
void serialBegin(uint8_t p, uint32_t c);
void serialEnd(uint8_t p);
void serialWrite(uint8_t p, uint8_t w);
void serialWriteList(uint8_t p, uint8_t *w8s);
uint32_t serialRead(uint8_t p);
uint8_t *serialReadList(uint8_t p);

#endif /* HaskinoRuntimeSerialH */
