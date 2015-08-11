#include <Arduino.h>
#include "AmberComm.h"
#include "AmberCommands.h"
#include "AmberScheduler.h"

typedef struct task_t 
    {
    struct task_t *next;
    struct task_t *prev;
    byte           id;
    uint16_t       size;
    uint16_t       currLen;
    uint16_t       currPos;
    uint32_t       millis;
    byte           data[];
    } TASK;

static int handleCreateTask(int size, byte *msg);
static int handleDeleteTask(int size, byte *msg);
static int handleAddToTask(int size, byte *msg);
static int handleScheduleTask(int size, byte *msg);
static int handleQuery(int size, byte *msg);
static int handleQueryAll(int size, byte *msg);
static int handleReset(int size, byte *msg);
static void deleteTask(TASK* task);
static TASK *findTask(int id);

static TASK *firstTask = NULL;
static int taskCount = 0;

int parseSchedulerMessage(int size, byte *msg)
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
            return handleQuery(size, msg);
            break;
        case SCHED_CMD_QUERY_ALL:
            return handleQueryAll(size, msg);
            break;
        case SCHED_CMD_RESET:
            return handleReset(size, msg);
            break;
        }
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

static int handleCreateTask(int size, byte *msg)
    {
    byte id = msg[1];
    uint16_t *taskSize = (uint16_t *) msg[2];
    TASK *newTask;

    if (!(findTask(id) != NULL ||
         ((newTask = (TASK *) malloc(*taskSize + sizeof(TASK))) == NULL )))
        {
        newTask->next = firstTask;
        newTask->prev = NULL;
        firstTask = newTask;
        newTask->id = id;
        newTask->size = *taskSize;
        newTask->currLen = 0;
        newTask->currPos = 0;
        taskCount++;
        }
    return 4;
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
    taskCount--;
    }

static int handleDeleteTask(int size, byte *msg)
    {
    byte id = msg[1];
    TASK *task;

    if ((task = findTask(id)) != NULL)
        {
        deleteTask(task);
        }
    return 2;
    }

static int handleAddToTask(int size, byte *msg)
    {
    byte id = msg[1];
    byte addSize = msg[2];
    byte *data = &msg[3];
    TASK *task;

    if ((task = findTask(id)) != NULL)
        {
        if (size + task->currLen <= task->size)
            {
            memcpy(&task->data[task->currLen], data, addSize);
            task->currLen += addSize;
            }
        }
    return 3 + addSize;
    }

static int handleScheduleTask(int size, byte *msg)
    {
    byte id = msg[1];
    uint32_t *deltaMillis = (uint32_t *) &msg[2];
    TASK *task;

    if ((task = findTask(id)) != NULL)
        {
        task->millis = millis() + *deltaMillis;
        }
    return 6;
    }

static int handleQuery(int size, byte *msg)
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
        *millisReply = task->millis;
        sendReply(sizeof(queryReply), SCHED_RESP_QUERY, queryReply);
        }
    else
        {
        sendReply(0, SCHED_RESP_QUERY, queryReply);
        }
    return 2;
    }

static int handleQueryAll(int size, byte *msg)
    {
    TASK *task = firstTask;

    startReplyFrame(SCHED_RESP_QUERY_ALL);
    sendReplyByte(taskCount);

    while(task)
        {
        sendReplyByte(task->id);
        task = task->next;
        }

    endReplyFrame();    
    return 1;
    }

static int handleReset(int size, byte *msg)
    {
    while(firstTask != NULL)
        {
        deleteTask(firstTask);
        }
    return 1;
    }

