/******************************************************************************
 *
 * Module      :  HaskinoRuntimeExpr
 * Copyright   :  (c) University of Kansas
 * License     :  BSD3
 * Stability   :  experimental
 *
 * Haskino Runtime Expression Evaluation module
 *****************************************************************************/

#include <Arduino.h>
#include <Servo.h>
#include <Stepper.h>
#include <Wire.h>
#include <math.h>
#include "HaskinoRuntime.h"
#include "HaskinoRuntimeList.h"

#define LITERAL_USE_COUNT   255

byte emptyList[] = {LITERAL_USE_COUNT,0};

// Show routines

byte *showBool(bool b)
    {
    byte *listMem;

    listMem = listAlloc(1+5+1);
    if (listMem)
        {
        if (b)
            listMem[1] = sprintf((char *) &listMem[2],"%s","True");
        else
            listMem[1] = sprintf((char *) &listMem[2],"%s","False");
        }
    return listMem;
    }

byte *showPinMode(uint32_t m)
    {
    byte *listMem;

    switch(m)
        {
        case 0:
            listMem = listAlloc(1+5+1);
            listMem[1] = sprintf((char *) &listMem[2],"%s","INPUT");
        case 1:
            listMem = listAlloc(1+6+1);
            listMem[1] = sprintf((char *) &listMem[2],"%s","OUTPUT");
        case 2:
            listMem = listAlloc(1+12+1);
            listMem[1] = sprintf((char *) &listMem[2],"%s","INPUT_PULLUP");
        default:
            listMem = listAlloc(1+7+1);
            listMem[1] = sprintf((char *) &listMem[2],"%s","Invalid");
        }
    return listMem;
    }

byte *showWord8(uint8_t w)
    {
    byte *listMem;

    listMem = listAlloc(1+3+1);
    if (listMem)
        {
        listMem[1] = sprintf((char *) &listMem[2],"%u",w);
        }
    return listMem;
    }

byte *showWord16(uint16_t w)
    {
    byte *listMem;

    listMem = listAlloc(1+5+1);
    if (listMem)
        {
        listMem[1] = sprintf((char *) &listMem[2],"%u",w);
        }
    return listMem;
    }

byte *showWord32(uint32_t w)
    {
    byte *listMem;

    listMem = listAlloc(1+10+1);
    if (listMem)
        {
        listMem[1] = sprintf((char *) &listMem[2],"%lu",w);
        }
    return listMem;
    }

byte *showInt8(int8_t i)
    {
    byte *listMem;

    listMem = listAlloc(1+4+1);
    if (listMem)
        {
        listMem[1] = sprintf((char *) &listMem[2],"%d",i);
        }
    return listMem;
    }

byte *showInt16(int16_t i)
    {
    byte *listMem;

    listMem = listAlloc(1+6+1);
    if (listMem)
        {
        listMem[1] = sprintf((char *) &listMem[2],"%d",i);
        }
    return listMem;
    }

byte *showInt32(int32_t i)
    {
    byte *listMem;

    listMem = listAlloc(1+11+1);
    if (listMem)
        {
        listMem[1] = sprintf((char *) &listMem[2],"%ld",i);
        }
    return listMem;
    }

byte *showFloat(float f, uint16_t w)
    {
    byte *listMem;

    listMem = listAlloc(1+11+1+w+1);
    if (listMem)
        {
        dtostrf(f, 4, w, (char *) &listMem[2]);
        listMem[1] = strlen((char *) &listMem[2]);
        }

    return listMem;
    }

// List functions

byte *listAlloc(int n)
    {
    byte *localMem;

    localMem = (byte *) haskinoMalloc(2+n);
    if (localMem)
        {
        localMem[0] = 0; // Ref count
        localMem[1] = 0; // Size
        }

    return localMem;
    }

void listFree(byte *l)
    {
    if (l != NULL && l[0] == 0)
        haskinoFree(l);
    }

void listAssign(byte **v, const byte *l)
    {
    byte *currList = *v;
    byte *listRefCnt = (byte *) l;

    // Decrement ref count of previously assigned list and free if needed
    if (currList != NULL && currList[0] != 0 && currList[0] != LITERAL_USE_COUNT)
        currList[0] = currList[0] - 1;
    listFree(currList);

    // Increment ref count of new assigned list
    if (listRefCnt[0] != LITERAL_USE_COUNT)
        listRefCnt[0] = listRefCnt[0] + 1;
    // Assign the list
    *v = (byte *) l;
    }

void listRelease(byte **v)
    {
    byte *currList = *v;

    if (currList != NULL && currList[0] != 0 && currList[0] != LITERAL_USE_COUNT)
        {
        currList[0] = currList[0] - 1;
        if (currList[0] == 0)
            {
            listFree(currList);
            *v = NULL;
            }
        }
    }

bool list8Less(byte *l1, byte *l2)
    {
    bool val;
    int l1len = l1[1];
    int l2len = l2[1];
    int i;

    for (i=0;
         i < l1len && i < l2len && l1[2+i] == l2[2+i];
         i++);
    if (i == l1len && i == l2len)
        val = false;
    else if (i == l1len)
        val = true;
    else if (i == l2len)
        val = false;
    else
        val = l1[2+i] < l2[2+i];

    listFree(l1);
    listFree(l2);
    return val;
    }

bool list8Equal(byte *l1, byte *l2)
    {
    bool val;
    int l1len = l1[1];
    int l2len = l2[1];

    if (l1len != l2len)
        val = false;
    else
        {
        val = true;
        for (int i=0;i<l1len;i++)
            {
            if (l1[2+i] != l2[2+i])
                {
                val = false;
                break;
                }
            }
        }

    listFree(l1);
    listFree(l2);
    return val;
    }

uint8_t list8Elem(uint8_t *l, uint8_t e)
    {
    uint8_t elem;

    if (e < l[1])
        elem =  l[2+e];
    else // ToDo: handle out of bound index
        elem = 0;

    listFree(l);
    return elem;
    }

uint8_t list8Len(uint8_t *l)
    {
    uint8_t len = l[1];

    listFree(l);
    return len;
    }

uint8_t *list8Cons(uint8_t w, uint8_t *l)
    {
    byte *newList;

    newList = listAlloc(l[1]+1);

    if (newList)
        {
        newList[1] = l[1] + 1;
        newList[2] = w;
        memcpy(&newList[3], &l[2], l[1]);
        }

    listFree(l);
    return newList;
    }

uint8_t *list8Reverse(uint8_t *l)
    {
    int i;
    int len = l[1];
    byte *newList;

    newList = listAlloc(len);

    if (newList)
        {
        newList[1] = len;
        for (i=0; i<len; i++)
            newList[i+2] = l[len - i + 1];
        }

    listFree(l);
    return newList;
    }

uint8_t *list8Apnd(uint8_t *l1, uint8_t *l2)
    {
    byte *newList;

    newList = listAlloc(l1[1]+l2[1]);

    if (newList)
        {
        newList[1] = l1[1] + l2[1];
        memcpy(&newList[2], &l1[2], l1[1]);
        memcpy(&newList[2+l1[1]], &l2[2], l2[1]);
        }

    listFree(l1);
    listFree(l2);
    return newList;
    }

uint8_t *list8Slice(uint8_t *l, uint8_t sindex, uint8_t len)
    {
    uint8_t *newList;
    uint8_t size = l[1];

    if (len == 0)
        {
        if (sindex < size)
            {
            size = size - sindex;
            }
        else
            {
            size = 0;
            }
        }
    else
        {
        if (sindex < size)
            {
            if (sindex + len < size)
                {
                size = len;
                }
            else
                {
                size = size - sindex;
                }
            }
        else
            {
            size = 0;
            }
        }

    newList = listAlloc(size);

    if (newList)
        {
        newList[1] = size;
        memcpy(&newList[2], &l[2+sindex], size);
        }

    listFree(l);
    return newList;
    }

// Bit functions

bool testBW8(uint8_t w, uint8_t b)
    {
    if (b > 7)
        return false;
    else
        return (w & ((uint8_t) 1 << b)) != 0;
    }

bool testBW16(uint16_t w, uint8_t b)
    {
    if (b > 15)
        return false;
    else
        return (w & ((uint16_t) 1 << b)) != 0;
    }

bool testBW32(uint32_t w, uint8_t b)
    {
    if (b > 31)
        return false;
    else
        return (w & ((uint32_t) 1 << b)) != 0;
    }

bool testBI8(int8_t w, uint8_t b)
    {
    if (b > 7)
        return false;
    else
        return (w & ((uint8_t) 1 << b)) != 0;
    }

bool testBI16(int16_t w, uint8_t b)
    {
    if (b > 15)
        return false;
    else
        return (w & ((uint16_t) 1 << b)) != 0;
    }

bool testBI32(int32_t w, uint8_t b)
    {
    if (b > 31)
        return false;
    else
        return (w & ((uint32_t) 1 << b)) != 0;
    }

uint8_t setBW8(uint8_t w, uint8_t b)
    {
    if (b > 7)
        return w;
    else
        return bitSet(w, b);
    }

uint16_t setBW16(uint16_t w, uint8_t b)
    {
    if (b > 7)
        return w;
    else
        return bitSet(w, b);
    }

uint32_t setBW32(uint32_t w, uint8_t b)
    {
    if (b > 7)
        return w;
    else
        return bitSet(w, b);
    }

int8_t setBI8(int8_t w, uint8_t b)
    {
    if (b > 7)
        return w;
    else
        return bitSet(w, b);
    }

int16_t setBI16(int16_t w, uint8_t b)
    {
    if (b > 15)
        return w;
    else
        return bitSet(w, b);
    }

int32_t setBI32(int32_t w, uint8_t b)
    {
    if (b > 32)
        return w;
    else
        return bitSet(w, b);
    }

uint8_t clrBW8(uint8_t w, uint8_t b)
    {
    if (b > 7)
        return w;
    else
        return bitClear(w, b);
    }

uint16_t clrBW16(uint16_t w, uint8_t b)
    {
    if (b > 15)
        return w;
    else
        return bitClear(w, b);
    }

uint32_t clrBW32(uint32_t w, uint8_t b)
    {
    if (b > 31)
        return w;
    else
        return bitClear(w, b);
    }

int8_t clrBI8(int8_t w, uint8_t b)
    {
    if (b > 7)
        return w;
    else
        return bitClear(w, b);
    }

int16_t clrBI16(int16_t w, uint8_t b)
    {
    if (b > 15)
        return w;
    else
        return bitClear(w, b);
    }

int32_t clrBI32(int32_t w, uint8_t b)
    {
    if (b > 31)
        return w;
    else
        return bitClear(w, b);
    }

// Sign functions

int8_t sign8(int8_t w)
    {
    int8_t val;

    if (w < 0)
        val = -1;
    else if (w == 0)
        val = 0;
    else
        val = 1;

    return val;
    }

int16_t sign16(int16_t w)
    {
    int16_t val;

    if (w < 0)
        val = -1;
    else if (w == 0)
        val = 0;
    else
        val = 1;

    return val;
    }

int32_t sign32(int32_t w)
    {
    int32_t val;

    if (w < 0)
        val = -1;
    else if (w == 0)
        val = 0;
    else
        val = 1;

    return val;
    }

float signF(float f)
    {
    float val;

    if (f < 0.0)
        val = -1.0;
    else if (f == 0.0)
        val = 0.0;
    else
        val = 1.0;

    return val;
    }

// Divide functions
int8_t div8(int8_t a, int8_t b)
    {
    int8_t c;

    c = a % b;
    if ((c != 0) && ((c < 0) != (b < 0)))
        return a / b - 1;
    else
        return a / b;
    }

int8_t mod8(int8_t a, int8_t b)
    {
    int8_t c;

    c = a % b;
    if ((c!=0) && ((c<0) != (b<0)))
        c += b;
    return c;
    }

int16_t div16(int16_t a, int16_t b)
    {
    int16_t c;

    c = a % b;
    if ((c != 0) && ((c < 0) != (b < 0)))
        return a / b - 1;
    else
        return a / b;
    }

int16_t mod16(int16_t a, int16_t b)
    {
    int16_t c;

    c = a % b;
    if ((c!=0) && ((c<0) != (b<0)))
        c += b;
    return c;
    }

int32_t div32(int32_t a, int32_t b)
    {
    int32_t c;

    c = a % b;
    if ((c != 0) && ((c < 0) != (b < 0)))
        return a / b - 1;
    else
        return a / b;
    }

int32_t mod32(int32_t a, int32_t b)
    {
    int32_t c;

    c = a % b;
    if ((c!=0) && ((c<0) != (b<0)))
        c += b;
    return c;
    }

// Float functions

float frac(float f)
    {
    float g;
    return modf(f, (double *) &g);
    }



