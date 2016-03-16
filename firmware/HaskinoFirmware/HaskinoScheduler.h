#ifndef HaskinoSchedulerH
#define HaskinoSchedulerH

#include "HaskinoConfig.h"

struct context_t;

typedef struct block_status_t
    {
    uint16_t currPos;
    } BLOCK_STATUS;

typedef struct task_t 
    {
    struct task_t      *next;
    struct task_t      *prev;
    struct context_t   *context;
    byte                id;
    uint16_t            size;
    uint16_t            currLen;
    uint16_t            currPos;
    uint32_t            millis;
    bool                rescheduled;
    byte               *endData;
    byte                data[];
    } TASK;

typedef struct context_t
    {
    TASK               *task;
    BLOCK_STATUS        blockStatus[MAX_BLOCK_LEVELS];
    int16_t             currBlockLevel;
    int16_t             recallBlockLevel;
    uint16_t            bindSize;
    byte               *bind;
    } CONTEXT;

bool parseSchedulerMessage(int size, const byte *msg, CONTEXT *context);
CONTEXT *schedulerDefaultContext();
void schedulerBootTask();
void schedulerRunTasks();
bool isRunningTask();
int getTaskCount();
void delayRunningTask(unsigned long ms);

#endif /* HaskinoSchedulerH */
