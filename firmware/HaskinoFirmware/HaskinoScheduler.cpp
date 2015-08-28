#include <Arduino.h>
#include "HaskinoComm.h"
#include "HaskinoCommands.h"
#include "HaskinoScheduler.h"

typedef struct task_t 
    {
    struct task_t *next;
    struct task_t *prev;
    byte           id;
    uint16_t       size;
    uint16_t       currLen;
    uint16_t       currPos;
    uint32_t       millis;
    byte          *endData;
    byte           data[];
    } TASK;

static bool handleCreateTask(int size, byte *msg);
static bool handleDeleteTask(int size, byte *msg);
static bool handleAddToTask(int size, byte *msg);
static bool handleScheduleTask(int size, byte *msg);
static bool handleQuery(int size, byte *msg, byte *local);
static bool handleQueryAll(int size, byte *msg, byte *local);
static bool handleReset(int size, byte *msg);
static void deleteTask(TASK* task);
static TASK *findTask(int id);
static bool executeTask(TASK *task);

static TASK *firstTask = NULL;
static TASK *runningTask = NULL;

bool parseSchedulerMessage(int size, byte *msg, byte *local)
    {
    switch (msg[0]) 
        {
        case SCHED_CMD_CREATE_TASK:
            return handleCreateTask(size, msg);
            break;
        case SCHED_CMD_DELETE_TASK:
            return handleDeleteTask(size, msg);
            break;
        case SCHED_CMD_ADD_TO_TASK:
            return handleAddToTask(size, msg);
            break;
        case SCHED_CMD_SCHED_TASK:
            return handleScheduleTask(size, msg);
            break;
        case SCHED_CMD_QUERY:
            return handleQuery(size, msg, local);
            break;
        case SCHED_CMD_QUERY_ALL:
            return handleQueryAll(size, msg, local);
            break;
        case SCHED_CMD_RESET:
            return handleReset(size, msg);
            break;
        }
    return false;
    }

static TASK *findTask(int id)
    {
    TASK *task = firstTask;

    while (task != NULL)
        {
        if (id == task->id)
            return task;
        task = task->next;
        }
    return NULL;
    }

static bool handleCreateTask(int size, byte *msg)
    {
    byte id = msg[1];
    unsigned int taskSize;
    memcpy(&taskSize, &msg[2], 2);
    TASK *newTask;

    if ((findTask(id) == NULL) &&
         ((newTask = (TASK *) malloc(taskSize + sizeof(TASK))) != NULL ))
        {
        newTask->next = firstTask;
        newTask->prev = NULL;
        firstTask = newTask;
        newTask->id = id;
        newTask->size = taskSize;
        newTask->currLen = 0;
        newTask->currPos = 0;
        newTask->endData = newTask->data + newTask->size;
        }
    return false;
    }

static void deleteTask(TASK* task)
    {
    if (task->prev != NULL)
        task->prev->next = task->next;
    else
        firstTask = task->next;
    if (task->next != NULL)
        task->next->prev = task->prev;
    free(task);
    }

static bool handleDeleteTask(int size, byte *msg)
    {
    byte id = msg[1];
    TASK *task;

    if ((task = findTask(id)) != NULL)
        {
        deleteTask(task);
        }
    return false;
    }

static bool handleAddToTask(int size, byte *msg)
    {
    byte id = msg[1];
    byte addSize = msg[2];
    byte *data = &msg[3];
    TASK *task;

    if ((task = findTask(id)) != NULL)
        {
        if (addSize + task->currLen <= task->size)
            {
            memcpy(&task->data[task->currLen], data, addSize);
            task->currLen += addSize;
            }
        }
    return false;
    }

static bool handleScheduleTask(int size, byte *msg)
    {
    byte id = msg[1];
    unsigned long deltaMillis;
    memcpy(&deltaMillis, &msg[2], 4);
    TASK *task;

    if ((task = findTask(id)) != NULL)
        {
        task->millis = millis() + deltaMillis;
        }
    return false;
    }

static bool handleQuery(int size, byte *msg, byte *local)
    {
    byte queryReply[10];
    uint16_t *sizeReply = (uint16_t *) queryReply;
    uint16_t *lenReply = (uint16_t *) &queryReply[2];
    uint16_t *posReply = (uint16_t *) &queryReply[4];
    uint32_t *millisReply = (uint32_t *) &queryReply[6];
    byte id = msg[1];
    TASK *task;

    if ((task = findTask(id)) != NULL)
        {
        *sizeReply = task->size;
        *lenReply = task->currLen;
        *posReply = task->currPos;
        *millisReply = task->millis - millis();
        sendReply(sizeof(queryReply), SCHED_RESP_QUERY, queryReply, local);
        }
    else
        {
        sendReply(0, SCHED_RESP_QUERY, queryReply, local);
        }
    return false;
    }

static bool handleQueryAll(int size, byte *msg, byte *local)
    {
    TASK *task = firstTask;

    startReplyFrame(SCHED_RESP_QUERY_ALL);

    while(task != NULL)
        {
        sendReplyByte(task->id);
        task = task->next;
        }
    // To Do - handle local

    endReplyFrame();    
    return false;
    }

static bool handleReset(int size, byte *msg)
    {
    while(firstTask != NULL)
        {
        deleteTask(firstTask);
        }
    return false;
    }

void schedulerRunTasks()
    {
    if (firstTask) 
        {
        unsigned long now = millis();
        TASK *current = firstTask;
        TASK *next = NULL;

        while (current) 
            {
            next = current->next;
            if (current->millis > 0 && current->millis < now) 
                { // ToDo: handle overflow
                runningTask = current;
                if (!executeTask(current)) 
                    {
                    deleteTask(current);
                    }
                runningTask = NULL;
                }
            current = next;
            }
        }
    }

static bool executeTask(TASK *task)
    {
    // Find end of next command
    bool taskRescheduled;

    while (task->currPos < task->currLen)
        {
        byte *msg = &task->data[task->currPos];
        byte cmdSize = msg[0];
        byte *cmd = &msg[1];

        taskRescheduled = parseMessage(cmdSize, cmd, NULL);  

        task->currPos += cmdSize + 1;
        if (taskRescheduled)
            {
            if (task->currPos >= task->currLen)
                {
                task->currPos = 0;
                }
            return true;
            }
        }
    return false;
    }

bool isRunningTask()
    {
    return runningTask != NULL;
    }

void delayRunningTask(unsigned long ms)
    {
    runningTask->millis += ms;   
    }
