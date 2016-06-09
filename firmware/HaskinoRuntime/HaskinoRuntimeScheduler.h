/******************************************************************************
 *
 * Module      :  HaskinoRuntimeScheduler
 * Copyright   :  (c) University of Kansas
 * License     :  BSD3
 * Stability   :  experimental
 *
 * Haskino Runtime Scheduler header
 *****************************************************************************/

#ifndef HaskinoRuntimeSchedulerH
#define HaskinoRuntimeSchedulerH

// Scheduling routines

void delayMilliseconds(uint32_t ms);
void createTask(uint8_t tid, void *tcb, int stackSize, void (*task)());
void deleteTask(uint8_t tid);
void scheduleTask(uint8_t tid, uint32_t tt);
void scheduleReset();
void taskComplete();
void startScheduler();

// Semphore routines

void giveSem(uint8_t id);
void takeSem(uint8_t id);

// Scheduler structures

typedef struct tcb_t 
    {
    struct tcb_t       *next;
    uint16_t            stackPointer;
    byte                id;
    void              (*entry)(void);
    uint16_t            stackSize;
    uint32_t            millis;
    bool                hasRan;
    bool                ready;
    byte                stack[];
    } TCB;

typedef struct semphore_c_t
    {
    bool full;
    TCB *waiting;
    } SEMAPHORE_C;

#endif /* HaskinoRuntimeSchedulerH */

