#include <Arduino.h>
#include <EEPROM.h>
#include "HaskinoComm.h"
#include "HaskinoCommands.h"
#include "HaskinoExpr.h"
#include "HaskinoScheduler.h"

#define BOOT_TASK_INDEX_START   6
#define BOOT_TASK_ID            255

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

static bool handleCreateTask(int size, const byte *msg);
static bool handleDeleteTask(int size, const byte *msg);
static bool handleAddToTask(int size, const byte *msg);
static bool handleScheduleTask(int size, const byte *msg);
static bool handleQuery(int size, const byte *msg, byte *local);
static bool handleQueryAll(int size, const byte *msg, byte *local);
static bool handleCreateTaskE(int size, const byte *msg);
static bool handleDeleteTaskE(int size, const byte *msg);
static bool handleAddToTaskE(int size, const byte *msg);
static bool handleScheduleTaskE(int size, const byte *msg);
static bool handleQueryE(int size, const byte *msg, byte *local);
static bool handleReset(int size, const byte *msg);
static bool handleBootTaskE(int size, const byte *msg);
static void deleteTask(TASK* task);
static TASK *findTask(int id);
static bool executeTask(TASK *task);
static bool deleteById(byte id);

static TASK *firstTask = NULL;
static TASK *runningTask = NULL;

bool parseSchedulerMessage(int size, const byte *msg, byte *local)
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
        case SCHED_CMD_CREATE_TASK_E:
            return handleCreateTaskE(size, msg);
            break;
        case SCHED_CMD_DELETE_TASK_E:
            return handleDeleteTaskE(size, msg);
            break;
        case SCHED_CMD_ADD_TO_TASK_E:
            return handleAddToTaskE(size, msg);
            break;
        case SCHED_CMD_SCHED_TASK_E:
            return handleScheduleTaskE(size, msg);
            break;
        case SCHED_CMD_QUERY_E:
            return handleQueryE(size, msg, local);
            break;
        case SCHED_CMD_RESET:
            return handleReset(size, msg);
            break;
         case SCHED_CMD_BOOT_TASK_E:
            return handleBootTaskE(size, msg);
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

static bool createById(byte id, unsigned int taskSize)
    {
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

static bool handleCreateTask(int size, const byte *msg)
    {
    byte id = msg[1];
    unsigned int taskSize;
    memcpy(&taskSize, &msg[2], 2);

    return createById(id, taskSize);
    }

static bool handleCreateTaskE(int size, const byte *msg)
    {
    byte *expr = (byte *) &msg[1];
    byte id = evalWord8Expr(&expr);
    unsigned int taskSize = evalWord16Expr(&expr);

    return createById(id, taskSize);
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

static bool deleteById(byte id)
    {
    TASK *task;

    if ((task = findTask(id)) != NULL)
        {
        deleteTask(task);
        }
    return false;
    }

static bool handleDeleteTask(int size, const byte *msg)
    {
    byte id = msg[1];
    return deleteById(id);
    }

static bool handleDeleteTaskE(int size, const byte *msg)
    {
    byte *expr = (byte *) &msg[1];
    byte id = evalWord8Expr(&expr);
    return deleteById(id);
    }

static bool addToTaskById(byte id, byte addSize, const byte *data)
    {
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

static bool handleAddToTask(int size, const byte *msg)
    {
    byte id = msg[1];
    byte addSize = msg[2];
    const byte *data = &msg[3];

    return addToTaskById(id, addSize, data);
    }

static bool handleAddToTaskE(int size, const byte *msg)
    {
    byte *expr = (byte *) &msg[1];
    byte id = evalWord8Expr(&expr);
    byte addSize = evalWord8Expr(&expr);
    const byte *data = expr;

    return addToTaskById(id, addSize, data);
    }

static bool scheduleById(byte id, unsigned long deltaMillis)
    {
    TASK *task;

    if ((task = findTask(id)) != NULL)
        {
        task->millis = millis() + deltaMillis;
        }
    return false;
    }

static bool handleScheduleTask(int size, const byte *msg)
    {
    byte id = msg[1];
    unsigned long deltaMillis;
    memcpy(&deltaMillis, &msg[2], 4);
    return scheduleById(id, deltaMillis);
    }

static bool handleScheduleTaskE(int size, const byte *msg)
    {
    byte *expr = (byte *) &msg[1];
    byte id = evalWord8Expr(&expr);
    unsigned long deltaMillis = evalWord32Expr(&expr);
    return scheduleById(id, deltaMillis);
    }

static bool queryById(byte id, byte *local)
    {
    byte queryReply[10];
    uint16_t *sizeReply = (uint16_t *) queryReply;
    uint16_t *lenReply = (uint16_t *) &queryReply[2];
    uint16_t *posReply = (uint16_t *) &queryReply[4];
    uint32_t *millisReply = (uint32_t *) &queryReply[6];
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

static bool handleQuery(int size, const byte *msg, byte *local)
    {
    byte id = msg[1];
    return queryById(id, local);
    }

static bool handleQueryE(int size, const byte *msg, byte *local)
    {
    byte *expr = (byte *) &msg[1];
    byte id = evalWord8Expr(&expr);
    return queryById(id, local);
    }

static bool handleQueryAll(int size, const byte *msg, byte *local)
    {
    TASK *task = firstTask;

    if (local)
        {
        while(task != NULL)
            {
            *local = task->id;
            task = task->next;
// ToDo Limit task reponse size??
            }
        }
    else
        {
        startReplyFrame(SCHED_RESP_QUERY_ALL);

        while(task != NULL)
            {
            sendReplyByte(task->id);
            task = task->next;
            }
        endReplyFrame();    
        }
    return false;
    }

static bool handleReset(int size, const byte *msg)
    {
    while(firstTask != NULL)
        {
        deleteTask(firstTask);
        }
    // Clear any stored task in EEPROM
    EEPROM[ 0 ] = 0;
    EEPROM[ 1 ] = 0;
    EEPROM[ 2 ] = 0;
    EEPROM[ 3 ] = 0;
    return false;
    }

static bool handleBootTaskE(int size, const byte *msg)
    {
    TASK *task;
    byte *expr = (byte *) &msg[1];
    byte id = evalWord8Expr(&expr);

    if ((task = findTask(id)) != NULL)
        {
        unsigned int index = BOOT_TASK_INDEX_START;

        EEPROM[ 0 ] = 'H';
        EEPROM[ 1 ] = 'A';
        EEPROM[ 2 ] = 'S';
        EEPROM[ 3 ] = 'K';
        EEPROM[ 4 ] = task->currLen & 0xFF;
        EEPROM[ 5 ] = task->currLen >> 8;

        for (int i=0;i<task->currLen;i++,index++)
            {
            EEPROM[ index ] = task->data[i];
            }
        }
    return false;
    }

void schedulerBootTask()
    {
    if (EEPROM[ 0 ] == 'H' && EEPROM[ 1 ] == 'A' &&
        EEPROM[ 2 ] == 'S' && EEPROM[ 3 ] == 'K')
        {
        TASK *task;
        uint16_t taskSize = EEPROM[ 4 ] | (EEPROM[ 5 ] << 8);
        int index = BOOT_TASK_INDEX_START;

        createById(BOOT_TASK_ID, taskSize);
        task = findTask(BOOT_TASK_ID);
        for (int i=0;i<taskSize;i++,index++)
            {
            task->data[i] = EEPROM[ index ];
            }
        scheduleById(BOOT_TASK_ID, 0);
        }
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
