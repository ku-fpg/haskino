#include <Arduino.h>
#include "HaskinoRuntime.h"

static void  *haskinoRefs[MAX_REFS];

bool readRefBool(int refIndex)
    {
    return (bool) haskinoRefs[refIndex];
    }

uint8_t readRefWord8(int refIndex)
    {
    return (uint8_t) (uint32_t) haskinoRefs[refIndex];
    }

uint16_t readRefWord16(int refIndex)
    {
    return (uint16_t) haskinoRefs[refIndex];
    }

uint32_t readRefWord32(int refIndex)
    {
    return (uint32_t) haskinoRefs[refIndex];
    }

int8_t readRefInt8(int refIndex)
    {
    return (int8_t) (uint32_t) haskinoRefs[refIndex];
    }

int16_t readRefInt16(int refIndex)
    {
    return (int16_t) haskinoRefs[refIndex];
    }

int32_t readRefInt32(int refIndex)
    {
    return (int32_t) haskinoRefs[refIndex];
    }

uint8_t *readRefList8(int refIndex)
    {
    return (uint8_t *) haskinoRefs[refIndex];
    }

float readRefFloat(int refIndex)
    {
    float *fVal;

    fVal = (float *) &haskinoRefs[refIndex];
    return *fVal;
    }

void storeBoolRef(byte refIndex, bool bVal)
    {
    haskinoRefs[refIndex] = (void *) bVal;
    }

void storeWord8Ref(byte refIndex, uint8_t w8Val)
    {
    haskinoRefs[refIndex] = (void *) (uint32_t) w8Val;
    }

void storeWord16Ref(byte refIndex, uint16_t w16Val)
    {
    haskinoRefs[refIndex] = (void *) (uint32_t) w16Val;
    }

void storeWord32Ref(byte refIndex, uint32_t w32Val)
    {
    haskinoRefs[refIndex] = (void *) w32Val;
    }

void storeInt8Ref(byte refIndex, int8_t i8Val)
    {
    haskinoRefs[refIndex] = (void *) (int32_t) i8Val;
    }

void storeInt16Ref(byte refIndex, int16_t i16Val)
    {
    haskinoRefs[refIndex] = (void *) (int32_t) i16Val;
    }

void storeInt32Ref(byte refIndex, int32_t i32Val)
    {
    haskinoRefs[refIndex] = (void *) i32Val;
    }

void storeFloatRef(byte refIndex, float fVal)
    {
    float *fPtr;

    fPtr = (float *) &haskinoRefs[refIndex];
    *fPtr = fVal;
    }

void storeList8Ref(byte refIndex, uint8_t *lVal)
    {
    listAssign((byte **) &haskinoRefs[refIndex], lVal);
    }

