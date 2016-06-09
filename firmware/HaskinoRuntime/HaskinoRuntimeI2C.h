/******************************************************************************
 *
 * Module      :  HaskinoRuntimeI2C
 * Copyright   :  (c) University of Kansas
 * License     :  BSD3
 * Stability   :  experimental
 *
 * Haskino Runtime I2C header
 *****************************************************************************/

#ifndef HaskinoRuntimeI2CH
#define HaskinoRuntimeI2CH

// I2C routines

void i2cWrite(uint8_t sa, uint8_t *w8s);
uint8_t *i2cRead(uint8_t sa, uint8_t n);
void i2cConfig();

#endif /* HaskinoRuntimeI2CH */
