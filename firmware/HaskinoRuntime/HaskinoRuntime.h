#ifndef HaskinoRuntimeH
#define HaskinoRuntimeH

typedef union ref_t
    {
    bool      boolVal;
    uint8_t   word8Val;
    uint16_t  word16Val;
    uint32_t  word32Val;
    int8_t    int8Val;
    int16_t   int16Val;
    int32_t   int32Val;
    byte      *list8Val;
    float     floatVal;
    }

// Show routines

byte *showBool(bool b);
byte *showWord8(uint8_t w);
byte *showWord16(uint16_t w);
byte *showWord32(uint32_t w);
byte *showInt8(int8_t i);
byte *showInt16(int16_t i);
byte *showInt32(int32_t i);
byte *showFloat(byte *f, uint16_t w);

// List functions

bool list8Less(byte *l);
bool list8Equal(byte *l);
uint8_t list8Elem(uint8_t *l, uint8_t e);
uint8_t list8Len(uint8_t *l);
uint8_t *list8Cons(uint8_t w, uint8_t *l);
uint8_t *list8Apnd(uint8_t *l1, uint8_t *l2);

// Bit functions

bool testBW8(uint8_t w, uint8_t b);
bool testBW16(uint16_t w, uint8_t b);
bool testBW32(uint32_t w, uint8_t b);
bool testBI8(int8_t w, uint8_t b);
bool testBI16(int16_t w, uint8_t b);
bool testBI32(int32_t w, uint8_t b);
uint8_t setBW8(uint8_t w, uint8_t b);
uint16_t setBW8(uint16_t w, uint8_t b);
uint32_t setBW8(uint32_t w, uint8_t b);
uint8_t setBI8(int8_t w), uint8_t b;
uint16_t setBI8(int16_t w, uint8_t b);
uint32_t setBI8(int32_t w, uint8_t b);
uint8_t clrBW8(uint8_t w, uint8_t b);
uint16_t clrBW8(uint16_t w, uint8_t b);
uint32_t clrBW8(uint32_t w, uint8_t b);
uint8_t clrBI8(int8_t w, uint8_t b);
uint16_t clrBI8(int16_t w, uint8_t b);
uint32_t clrBI8(int32_t w, uint8_t b);

// Sign functions
int8_t sign8(int8_t w);
int16_t sign16(int16_t w);
int32_t sign32(int32_t w);
float signF(float f);

// Divide functions
int8_t div8(int8_t a, int8_t b);
int8_t mod8(int8_t a, int8_t b);
int16_t div16(int16_t a, int16_t b);
int16_t mod16(int16_t a, int16_t b);
int32_t div32(int32_t a, int32_t b);
int32_t mod32(int32_t a, int32_t b);

// Float functions
float frac(float f);

#endif /* HaskinoRuntimeH */

