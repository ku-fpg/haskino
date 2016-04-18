#ifndef HaskinoRuntimeH
#define HaskinoRuntimeH

#include "../HaskinoFirmware/HaskinoFirmware.h"
#include "../HaskinoFirmware/HaskinoBoardStatus.h"
#include "../HaskinoFirmware/HaskinoConfig.h"

// Platform Query routines

uint16_t queryFirmware();
uint8_t queryProcessor();

// Digital port routines

void digitalPortWrite(uint8_t p, uint8_t b, uint8_t m);
uint8_t digitalPortRead(uint8_t p, uint8_t m);

// I2C routines

void i2cWrite(uint8_t sa, uint8_t *w8s);
uint8_t *i2cRead(uint8_t sa, uint8_t n);
void i2cConfig();

// Stepper routines

uint8_t stepper2Pin(uint16_t s, uint8_t p1, uint8_t p2);
uint8_t stepper4Pin(uint16_t s, uint8_t p1, uint8_t p2, uint8_t p3, uint8_t p4);
void stepperSetSpeed(uint8_t st, int32_t sp);

// Servo routines

uint8_t servoAttach(uint8_t p);
uint8_t servoAttachMinMax(uint8_t p, uint16_t min, uint16_t max);
void servoDetach(uint8_t sv);
void servoWrite(uint8_t sv, uint16_t deg);
void servoWriteMicros(uint8_t sv, uint16_t m);
uint16_t servoRead(uint8_t sv);
uint16_t servoReadMicros(uint8_t sv);

// Scheduling reoutines

void delayMilliseconds(uint32_t ms);
void createTask(uint8_t tid, void (*task)());
void deleteTask(uint8_t tid);
void scheduleTask(uint8_t tid, uint32_t tt);
void scheduleReset();
void attachInt(uint8_t p, uint8_t t, uint8_t m);
void detachInt(uint8_t p);

// Semphore routines

void giveSem(uint8_t id);
void takeSem(uint8_t id);

// Debug routines

void debug(uint8_t s);

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

bool list8Less(byte *l1, byte *l2);
bool list8Equal(byte *l1, byte *l2);
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
uint16_t setBW16(uint16_t w, uint8_t b);
uint32_t setBW32(uint32_t w, uint8_t b);
int8_t setBI8(int8_t w, uint8_t b);
int16_t setBI16(int16_t w, uint8_t b);
int32_t setBI32(int32_t w, uint8_t b);
uint8_t clrBW8(uint8_t w, uint8_t b);
uint16_t clrBW16(uint16_t w, uint8_t b);
uint32_t clrBW32(uint32_t w, uint8_t b);
int8_t clrBI8(int8_t w, uint8_t b);
int16_t clrBI16(int16_t w, uint8_t b);
int32_t clrBI32(int32_t w, uint8_t b);

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

