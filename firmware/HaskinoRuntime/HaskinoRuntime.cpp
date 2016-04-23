#include <Arduino.h>
#include <Servo.h>
#include <Stepper.h>
#include <Wire.h>
#include <math.h>
#include "HaskinoRuntime.h"


static byte *listAlloc(int n);
static void listFree(byte *l);
void listAssign(byte **v, byte *l);

// Platform Query routines

uint16_t queryFirmware()
    {
    return (FIRMWARE_MAJOR << 8) | FIRMWARE_MINOR;
    }

uint8_t queryProcessor()
    {
    static uint8_t type = 
#if defined(__AVR_ATmega8__)
                                ATmega8_TYPE;
#elif defined(__AVR_ATmega168__)
                                ATmega168_TYPE;
#elif defined(__AVR_ATmega328P__)
                                ATmega328P_TYPE;
#elif defined(__AVR_ATmega1280__)
                                ATmega1280_TYPE;
#elif defined(__AVR_ATmega2560__)
                                ATmega256_TYPE;
#elif defined(__AVR_ATmega32U4__)
                                ATmega32U4_TYPE;
#elif defined(__AVR_ATmega644P__)
                                ATmega644P_TYPE;
#elif defined(__AVR_ATmega644__)
                                ATmega644_TYPE;
#elif defined(__AVR_ATmega645__)
                                ATmega645_TYPE;
#elif defined(__SAM3X8E__)
                                SAM3X8E_TYPE;
#elif defined(ARDUINO_LINUX)
                                X86_TYPE;
#elif defined(INTEL_EDISON)
                                QUARK_TYPE;
#else
#error "Please define a new processor type board"
#endif

    return type;
    }
    
// Digital port routines

static uint8_t bits[8] = {0x01,0x02,0x04,0x08,0x10,0x20,0x40,0x80};

void digitalPortWrite(uint8_t pinNo, uint8_t value, uint8_t mask)
    {
    for (int i=0;i<8;i++)
        {
        if (bits[i] & mask) 
            {
            digitalWrite(pinNo+i, (bits[i] & value) == bits[i]);
            }
        }
    }
    
uint8_t digitalPortRead(uint8_t pinNo, uint8_t mask)
    {
    uint8_t value = 0;

    for (int i=0;i<8;i++)
        {
        if ((bits[i] & mask) && digitalRead(pinNo+i))
            value |= bits[i];
        }

    return (value);
    }
    
// I2C routines

void i2cWrite(uint8_t sa, uint8_t *w8s)
    {
    if (w8s[1] != 0)
        {
        Wire.beginTransmission(sa);
        Wire.write(&w8s[2], w8s[1]);
        Wire.endTransmission();
        delayMicroseconds(70);
        }
    listFree(w8s);
    }
    
uint8_t *i2cRead(uint8_t sa, uint8_t byteCount)
    {
    byte *localMem, *local;
    int byteAvail;

    Wire.requestFrom((int) sa, (int) byteCount);
    byteAvail = Wire.available();

    localMem = listAlloc(byteAvail);
    if (localMem == NULL)
        return NULL;

    local = &localMem[2];

    localMem[1] = byteAvail;

    for (int i = 0; i < byteAvail; i++)
        { 
        *local++ = Wire.read();
        }

    return localMem;
    }
    
void i2cConfig()
    {
    Wire.begin();
    delay(10);
    }
    
// Stepper routines

static Stepper *steppers[MAX_FIRM_STEPPERS];

static int nextStepper = 0;

uint8_t stepper2Pin(uint16_t steps, uint8_t p1, uint8_t p2)
    {
    Stepper *newStepper;
    uint8_t stepperIndex;

    newStepper = new Stepper(steps, p1, p2);
    stepperIndex = nextStepper;

    steppers[nextStepper++] = newStepper;
    return stepperIndex;
    }
    
uint8_t stepper4Pin(uint16_t steps, uint8_t p1, uint8_t p2, 
                    uint8_t p3, uint8_t p4)
    {
    Stepper *newStepper;
    uint8_t stepperIndex;

    newStepper = new Stepper(steps, p1, p2, p3, p4);
    stepperIndex = nextStepper;

    steppers[nextStepper++] = newStepper;
    return stepperIndex;
    }
    
void stepperSetSpeed(uint8_t st, int32_t sp)
    {
    steppers[st]->setSpeed(sp);
    }
    
// Servo routines
#define DEFAULT_SERVO_MIN 544
#define DEFAULT_SERVO_MAX 2400

static Servo *servos[MAX_FIRM_SERVOS];

static int nextServo = 0;

uint8_t servoAttach(uint8_t pin)
    {
    return servoAttachMinMax(pin, DEFAULT_SERVO_MIN, DEFAULT_SERVO_MAX);
    }
    
uint8_t servoAttachMinMax(uint8_t pin, uint16_t min, uint16_t max)
    {
    Servo *newServo;
    uint8_t servoIndex;

    newServo = new Servo();
    newServo->attach(pin, min, max);
    servoIndex = nextServo;

    servos[nextServo++] = newServo;

    return servoIndex;
    }
    
void servoDetach(uint8_t sv)
    {
    servos[sv]->detach();
    }
    
void servoWrite(uint8_t sv, uint16_t deg)
    {
    servos[sv]->write(deg);
    }
    
void servoWriteMicros(uint8_t sv, uint16_t micros)
    {
    servos[sv]->writeMicroseconds(micros);
    }
    
uint16_t servoRead(uint8_t sv)
    {
    return servos[sv]->read();
    }
    
uint16_t servoReadMicros(uint8_t sv)
    {
    return servos[sv]->readMicroseconds();
    }
    
// Scheduling routines
#define DEFAULT_TASK_STACK_SIZE 64
#define CONTEXT_TASK_STACK_SIZE 36

static TCB *firstTask = NULL;
static TCB *runningTask = NULL;
static int taskCount = 0;
static SEMAPHORE_C semaphores[NUM_SEMAPHORES];

// Temp variables used for C/Assembly transfer
volatile uint32_t taskStack;
volatile uint32_t taskFunction;

// Macros to save and restore context
#define SAVE_TASK_CONTEXT()\
asm volatile (\
"push r0                \n\t"\
"in r0, __SREG__        \n\t"\
"cli                    \n\t"\
"push r0                \n\t"\
"push r1                \n\t"\
"clr r1                 \n\t"\
"push r2                \n\t"\
"push r3                \n\t"\
"push r4                \n\t"\
"push r5                \n\t"\
"push r6                \n\t"\
"push r7                \n\t"\
"push r8                \n\t"\
"push r9                \n\t"\
"push r10               \n\t"\
"push r11               \n\t"\
"push r12               \n\t"\
"push r13               \n\t"\
"push r14               \n\t"\
"push r15               \n\t"\
"push r16               \n\t"\
"push r17               \n\t"\
"push r18               \n\t"\
"push r19               \n\t"\
"push r20               \n\t"\
"push r21               \n\t"\
"push r22               \n\t"\
"push r23               \n\t"\
"push r24               \n\t"\
"push r25               \n\t"\
"push r26               \n\t"\
"push r27               \n\t"\
"push r28               \n\t"\
"push r29               \n\t"\
"push r30               \n\t"\
"push r31               \n\t"\
"in r26, __SP_L__       \n\t"\
"in r27, __SP_H__       \n\t"\
"sts taskStack+1, r27   \n\t"\
"sts taskStack, r26     \n\t"\
"sei                    \n\t" : :);

#define LOAD_TASK_CONTEXT()\
asm volatile (\
"cli                    \n\t"\
"out __SP_L__, %A0      \n\t"\
"out __SP_H__, %B0      \n\t"\
"pop r31                \n\t"\
"pop r30                \n\t"\
"pop r29                \n\t"\
"pop r28                \n\t"\
"pop r27                \n\t"\
"pop r26                \n\t"\
"pop r25                \n\t"\
"pop r24                \n\t"\
"pop r23                \n\t"\
"pop r22                \n\t"\
"pop r21                \n\t"\
"pop r20                \n\t"\
"pop r19                \n\t"\
"pop r18                \n\t"\
"pop r17                \n\t"\
"pop r16                \n\t"\
"pop r15                \n\t"\
"pop r14                \n\t"\
"pop r13                \n\t"\
"pop r12                \n\t"\
"pop r11                \n\t"\
"pop r10                \n\t"\
"pop r9                 \n\t"\
"pop r8                 \n\t"\
"pop r7                 \n\t"\
"pop r6                 \n\t"\
"pop r5                 \n\t"\
"pop r4                 \n\t"\
"pop r3                 \n\t"\
"pop r2                 \n\t"\
"pop r1                 \n\t"\
"pop r0                 \n\t"\
"sei                    \n\t"\
"out __SREG__, r0       \n\t"\
"pop r0                 \n\t": : "r" (taskStack))

#define INIT_TASK_STACK()\
asm volatile(\
"out __SP_L__, %A0      \n\t"\
"out __SP_H__, %B0      \n\t": : "r" (taskStack))

#if defined(__AVR_ATmega1280__) || defined(__AVR_ATmega2560__)
    #define INIT_TASK_PTR()\
    asm volatile(\
    "mov r0, %A0        \n\t"\
    "push r0            \n\t"\
    "mov r0, %B0        \n\t"\
    "push r0            \n\t"\
    "mov r0, %C0        \n\t"\
    "push r0            \n\t": : "r" (taskFunction))
#else
    #define INIT_TASK_PTR()\
    asm volatile(\
    "mov r0, %A0        \n\t"\
    "push r0            \n\t"\
    "mov r0, %B0        \n\t"\
    "push r0            \n\t": : "r" (taskFunction))
#endif

static TCB *findTask(int id)
    {
    TCB *task = firstTask;

    while (task != NULL)
        {
        if (id == task->id)
            return task;
        task = task->next;
        }
    return NULL;
    }

void delayMilliseconds(uint32_t ms)
    {
    runningTask->millis = millis() + ms;
    reschedule();
    }
    
void createTask(uint8_t tid, int bindCount, void (*task)())
    {
    TCB *newTask;
    int stackSize = (bindCount * 4) + DEFAULT_TASK_STACK_SIZE + 
                    CONTEXT_TASK_STACK_SIZE;
       
    if ((findTask(tid) == NULL) &&
         ((newTask = (TCB *) malloc(stackSize + sizeof(TCB))) != NULL ))
        {
        newTask->next = firstTask;
        firstTask = newTask;
        newTask->id = tid;
        newTask->stackSize = stackSize;
        newTask->millis = 0;
        newTask->ready = false;
        newTask->hasRan = false;
        newTask->entry = task;
        newTask->stackPointer = 
            (uint16_t) &newTask->stack[DEFAULT_TASK_STACK_SIZE-1];
        taskCount++;
        }
    }
    
void deleteTask(uint8_t tid)
    {
    TCB *task;

    if ((task = findTask(tid)) != NULL)
        {
        task->ready = false;
        if (task == runningTask)
            reschedule();
        }
    }
    
void scheduleTask(uint8_t tid, uint32_t tt)
    {
    TCB *task;

    if ((task = findTask(tid)) != NULL)
        {
        task->millis = millis() + tt;
        task->ready = true;
        }
    }
    
void scheduleReset()
    {
    TCB *task = firstTask;

    // Set all tasks except for running one to false
    while (task != NULL)
        {
        if (task != runningTask)
            task->ready = false;
        task = task->next;
        }
    }
    
void taskComplete()
    {
    runningTask->ready = false;
    reschedule();
    }

void switchTo(TCB *newTask)
    {
    SAVE_TASK_CONTEXT();
    runningTask->stackPointer = taskStack;

    runningTask = newTask;
    if(!runningTask->hasRan)
        {
        runningTask->hasRan = true;
        taskStack = (uint32_t) runningTask->stackPointer;
        INIT_TASK_STACK();
        taskFunction = (uint32_t) runningTask->entry;
        INIT_TASK_PTR();
        }
    else
        {
        LOAD_TASK_CONTEXT();
        }           
    asm("ret");
    }

void startScheduler()
    {
    TCB *task;

    task = findTask(255);
    runningTask = task;
    switchTo(task);
    }

void reschedule()
    {
    TCB *next = runningTask->next;

    if (next == NULL)
        next = firstTask;

    while (true) 
        {
        unsigned long now;

        now = millis();

        if (next->ready && 
            now - next->millis < 0x80000000UL)
            {
            if (next != runningTask)
                {
                switchTo(next);
                }
            return;
            }

        if (next->next == NULL)
            next = firstTask;
        else
            next = next->next;
        }
    }

void attachInt(uint8_t p, uint8_t t, uint8_t m)
    {
    // ToDo: Fill in with scheduling
    }
    
void detachInt(uint8_t p)
    {
    // ToDo: Fill in with scheduling
    }
    

// Semphore routines

void giveSem(uint8_t id)
    {
    if (id < NUM_SEMAPHORES)
        {
        // Semaphore is already full, do nothing
        if (semaphores[id].full)
            {
            }
        // Semaphore has a task waiting, ready it to run 
        else if (semaphores[id].waiting)
            {
            TCB *task = semaphores[id].waiting;

            task->ready = true;
            task->millis = millis();
            semaphores[id].waiting = NULL;
            }
        // Otherwise mark the semphore as full
        else
            {
            semaphores[id].full = true;
            }
        }
    }
    
void takeSem(uint8_t id)
    {
    if (id < NUM_SEMAPHORES)
        {
        // Semaphore is already full, take it and do not reschedule
        if (semaphores[id].full)
            {
            semaphores[id].full = false;
            }
        else
            // Semaphore is not full, we need to add ourselves to waiting
            // and reschedule
            {
            semaphores[id].waiting = runningTask;
            runningTask->ready = false;
            }
        }
    }
    
// Debug routines

void debug(uint8_t *s)
    {
    bool opened = false;

    if (!opened)
        {
        Serial.begin(115200);
        opened = true;
        }

    for (int i=0; i<s[1]; i++)
        Serial.write(s[2+i]);
    Serial.write('\n');
    listFree(s);
    }
    
// Show routines

byte *showBool(bool b)
    {
    byte *listMem;

    listMem = listAlloc(5+1);
    if (listMem)
        {
        if (b)
            listMem[1] = sprintf((char *) &listMem[2],"%s","True");
        else
            listMem[1] = sprintf((char *) &listMem[2],"%s","False");
        }
    return listMem;
    }

byte *showWord8(uint8_t w)
    {
    byte *listMem;

    listMem = listAlloc(3+1);
    if (listMem)
        {
        listMem[1] = sprintf((char *) &listMem[2],"%u",w);
        }
    return listMem;
    }

byte *showWord16(uint16_t w)
    {
    byte *listMem;

    listMem = listAlloc(5+1);
    if (listMem)
        {
        listMem[1] = sprintf((char *) &listMem[2],"%u",w);
        }
    return listMem;
    }

byte *showWord32(uint32_t w)
    {
    byte *listMem;

    listMem = listAlloc(10+1);
    if (listMem)
        {
        listMem[1] = sprintf((char *) &listMem[2],"%lu",w);
        }
    return listMem;
    }

byte *showInt8(int8_t i)
    {
    byte *listMem;

    listMem = listAlloc(4+1);
    if (listMem)
        {
        listMem[1] = sprintf((char *) &listMem[2],"%d",i);
        }
    return listMem;
    }

byte *showInt16(int16_t i)
    {
    byte *listMem;

    listMem = listAlloc(6+1);
    if (listMem)
        {
        listMem[1] = sprintf((char *) &listMem[2],"%d",i);
        }
    return listMem;
    }

byte *showInt32(int32_t i)
    {
    byte *listMem;

    listMem = listAlloc(11+1);
    if (listMem)
        {
        listMem[1] = sprintf((char *) &listMem[2],"%ld",i);
        }
    return listMem;
    }

byte *showFloat(float f, uint16_t w)
    {
    byte *listMem;

    listMem = listAlloc(11+1+w+1);
    if (listMem)
        {
        dtostrf(f, 4, w, (char *) &listMem[2]);
        listMem[1] = strlen((char *) &listMem[2]);
        }

    return listMem;
    }

// List functions

static byte *listAlloc(int n)
    {
    byte *localMem;

    localMem = (byte *) malloc(2+n);
    if (localMem)
        {
        localMem[0] = 0; // Ref count
        localMem[1] = 0; // Size
        }

    return localMem;
    }

static void listFree(byte *l)
    {
    if (l != NULL && l[0] == 0)
        free(l);
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
    if (e < l[1])
        return l[2+e];
    else // ToDo: handle out of bound index
        return 0;
    }

uint8_t list8Len(uint8_t *l)
    {
    return l[1];
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



