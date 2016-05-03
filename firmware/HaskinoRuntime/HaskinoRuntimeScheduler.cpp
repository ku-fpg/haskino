/******************************************************************************
 *
 * Module      :  HaskinoRuntimeScheduler
 * Copyright   :  (c) University of Kansas
 * License     :  BSD3
 * Stability   :  experimental
 *
 * Haskino Runtime Scheduler module
 *****************************************************************************/

#include <Arduino.h>
#include "HaskinoRuntime.h"

// Scheduling routines
static void reschedule();
static void switchTo(TCB *newTask);
static bool minWait(TCB *task);
static void reschedule();

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

#if defined(__AVR_ATmega1280__) || defined(__AVR_ATmega2560__)
    #define INIT_TASK_STACK_FUNC()\
    asm volatile(\
    "out __SP_L__, %A0  \n\t"\
    "out __SP_H__, %B0  \n\t"\
    "mov r0, %A1        \n\t"\
    "push r0            \n\t"\
    "mov r0, %B1        \n\t"\
    "push r0            \n\t"\
    "mov r0, %C1        \n\t"\
    "push r0            \n\t" : : "r" (taskStack), "r" (taskFunction))
#else
    #define INIT_TASK_STACK_FUNC()\
    asm volatile(\
    "out __SP_L__, %A0  \n\t"\
    "out __SP_H__, %B0  \n\t"\
    "mov r0, %A1        \n\t"\
    "push r0            \n\t"\
    "mov r0, %B1        \n\t"\
    "push r0            \n\t" : : "r" (taskStack), "r" (taskFunction))
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
#if 1
    runningTask->millis = millis() + ms;
    reschedule();
#else
    delay(ms);
#endif
    }
    
void createTask(uint8_t tid, void *tcb, int stackSize, void (*task)())
    {
    TCB *newTask = (TCB *) tcb;
       
    if (findTask(tid) == NULL)
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
            (uint16_t) &newTask->stack[stackSize-sizeof(unsigned long)];
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

static void switchTo(TCB *newTask)
    {
    SAVE_TASK_CONTEXT();
    runningTask->stackPointer = taskStack;

    runningTask = newTask;
    if(!runningTask->hasRan)
        {
        runningTask->hasRan = true;
        taskStack = (uint32_t) runningTask->stackPointer;
        taskFunction = (uint32_t) runningTask->entry;
        INIT_TASK_STACK_FUNC();
        }
    else
        {
        taskStack = (uint32_t) runningTask->stackPointer;
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

static bool minWait(TCB *task)
    {
    uint32_t now = millis();
    uint32_t minTime = 0x80000000UL;
    TCB *currTask = task;
    TCB *minTask = NULL;

    // Find the task that is ready to run and has the shortest delay
    do  {
        if (currTask->ready)
            {
            uint32_t timeDiff = currTask->millis - now;

            if (timeDiff >= 0x80000000UL)
                timeDiff = 0;
            if (timeDiff < minTime)
                {
                minTime = timeDiff;
                minTask = currTask;
                }
            }

        if (currTask->next == NULL)
            currTask = firstTask;
        else
            currTask = currTask->next;

        } while (currTask != task);

    // If one was found, then delay until its delay would time out
    // and switch to that task.
    if (minTask)
        {
        delay(minTime);
        if (minTask != runningTask)
            switchTo(minTask);
        return false;
        }
    else
        {
        return true;
        }
    }

static void reschedule()
    {
    TCB *next = runningTask->next;

    if (next == NULL)
        next = firstTask;

    // Loop while no tasks are ready.
    // Start the search with the task after the current one, so that
    // if all tasks used delayMillis(0), they would proceed in a 
    // round robin fashion.

    while (minWait(next)) 
        {
        delay(1);
        }
    }

// Critical Region global lock routines

static inline uint8_t lock()
    {
    uint8_t statReg = SREG;
    cli();
    return statReg;
    }

static inline void unlock(uint8_t statReg)
    {
    SREG = statReg;
    }

// Semphore routines

void giveSem(uint8_t id)
    {
    if (id < NUM_SEMAPHORES)
        {
        uint8_t reg;

        reg = lock();
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
        unlock(reg);
        }
    }
    
void takeSem(uint8_t id)
    {
    if (id < NUM_SEMAPHORES)
        {
        uint8_t reg;

        reg = lock();
        // Semaphore is already full, take it and do not reschedule
        if (semaphores[id].full)
            {
            semaphores[id].full = false;
            unlock(reg);
            }
        else
            // Semaphore is not full, we need to add ourselves to waiting
            // and reschedule
            {
            semaphores[id].waiting = runningTask;
            runningTask->ready = false;
            unlock(reg);
            reschedule();
            }
        }
    }
