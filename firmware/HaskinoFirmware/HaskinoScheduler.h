#ifndef HaskinoSchedulerH
#define HaskinoSchedulerH

struct context_t;

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
    byte               *endData;
    byte                data[];
    } TASK;

typedef struct context_t
    {
    TASK               *task;
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
