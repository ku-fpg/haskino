/******************************************************************************
 *
 * Module      :  HaskinoRuntimeRefs
 * Copyright   :  (c) University of Kansas
 * License     :  BSD3
 * Stability   :  experimental
 *
 * Haskino Runtime Expresion Evaluation handling header
 *****************************************************************************/

#ifndef HaskinoRuntimeRefsH
#define HaskinoRuntimeRefsH

bool readRefBool(int refIndex);
uint8_t readRefWord8(int refIndex);
uint16_t readRefWord16(int refIndex);
uint32_t readRefWord32(int refIndex);
int8_t readRefInt8(int refIndex);
int16_t readRefInt16(int refIndex);
int32_t readRefInt32(int refIndex);
uint8_t *readRefList8(int refIndex);
float readRefFloat(int refIndex);
void storeBoolRef(byte refIndex, bool bVal);
void storeWord8Ref(byte refIndex, uint8_t w8Val);
void storeWord16Ref(byte refIndex, uint16_t w16Val);
void storeWord32Ref(byte refIndex, uint32_t w32Val);
void storeInt8Ref(byte refIndex, int8_t i8Val);
void storeInt16Ref(byte refIndex, int16_t i16Val);
void storeInt32Ref(byte refIndex, int32_t i32Val);
void storeFloatRef(byte refIndex, float fVal);
void storeList8Ref(byte refIndex, uint8_t *lVal);

#endif /* HaskinoRuntimeRefsH */
