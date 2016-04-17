#include <Arduino.h>
#include "HaskinoRuntime.h"

// Show routines

byte *showBool(bool b)
    {
    byte *listMem;

    listMem = (byte *) malloc(2+5+1);
    if (listMem)
        {
        if (b)
            listMem[1] = sprintf((char *) &listMem[1],"%s","True");
        else
            listMem[1] = sprintf((char *) &listMem[1],"%s","False");
        }
    return listMem;
    }

byte *showWord8(uint8_t w)
    {
    byte *listMem;

    listMem = (byte *) malloc(2+3+1);
    if (listMem)
        {
        listMem[1] = sprintf((char *) &listMem[1],"%u",w);
        }
    return listMem;
    }

byte *showWord16(uint16_t w)
    {
    byte *listMem;

    listMem = (byte *) malloc(2+5+1);
    if (listMem)
        {
        listMem[1] = sprintf((char *) &listMem[1],"%u",w);
        }
    return listMem;
    }

byte *showWord32(uint32_t w)
    {
    byte *listMem;

    listMem = (byte *) malloc(2+10+1);
    if (listMem)
        {
        listMem[1] = sprintf((char *) &listMem[1],"%lu",w);
        }
    return listMem;
    }

byte *showInt8(int8_t i)
    {
    byte *listMem;

    listMem = (byte *) malloc(2+4+1);
    if (listMem)
        {
        listMem[1] = sprintf((char *) &listMem[1],"%d",i);
        }
    return listMem;
    }

byte *showInt16(int16_t i)
    {
    byte *listMem;

    listMem = (byte *) malloc(2+6+1);
    if (listMem)
        {
        listMem[1] = sprintf((char *) &listMem[1],"%d",i);
        }
    return listMem;
    }

byte *showInt32(int32_t i)
    {
    byte *listMem;

    listMem = (byte *) malloc(2+11+1);
    if (listMem)
        {
        listMem[1] = sprintf((char *) &listMem[1],"%ld",i);
        }
    return listMem;
    }

byte *showFloat(float f, uint16_t w)
    {
    byte *listMem;

    listMem = (byte *) malloc(2+11+1+w+1);
    if (listMem)
        {
        dtostrf(f, 4, w, (char *) &listMem[1]);
        listMem[1] = strlen((char *) &listMem[1]);
        }

    return listMem;
    }

// List functions

bool list8Less(byte *l)
    {
    return false;  // ToDo: Fill in
    }

bool list8Equal(byte *l)
    {
    return false;  // ToDo: Fill in
    }

uint8_t list8Elem(uint8_t *l, uint8_t e)
    {
    if (e < l[0])
        return l[1+e];
    else // ToDo: handle out of bound index
        return 0;
    }

uint8_t list8Len(uint8_t *l)
    {
    return l[1];
    }

uint8_t *list8Cons(uint8_t w, uint8_t *l)
    {
    return NULL;  // ToDo: Fill in
    }

uint8_t *list8Apnd(uint8_t *l1, uint8_t *l2)
    {
    return NULL;  // ToDo: Fill in
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



