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
    byte          *endData;
    byte           data[];
    } TASK;

static bool handleCreateTask(int size, byte *msg);
static bool handleDeleteTask(int size, byte *msg);
static bool handleAddToTask(int size, byte *msg);
static bool handleScheduleTask(int size, byte *msg);
static bool handleQuery(int size, byte *msg);
static bool handleQueryAll(int size, byte *msg);
static bool handleReset(int size, byte *msg);
static void deleteTask(TASK* task);
static TASK *findTask(int id);
static bool executeTask(TASK *task);

static TASK *firstTask = NULL;

bool parseSchedulerMessage(int size, byte *msg)
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
    uint16_t taskSize = msg[2] + (msg[3] << 8);
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
        if (size + task->currLen <= task->size)
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
    uint32_t deltaMillis = msg[2] + (msg[3] << 8) + 
                           (msg[4] << 16) + (msg[5] << 24);
    TASK *task;

    if ((task = findTask(id)) != NULL)
        {
        task->millis = millis() + deltaMillis;
        }
    return false;
    }

static bool handleQuery(int size, byte *msg)
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
    return false;
    }

static bool handleQueryAll(int size, byte *msg)
    {
    TASK *task = firstTask;

    startReplyFrame(SCHED_RESP_QUERY_ALL);

    while(task != NULL)
        {
        sendReplyByte(task->id);
        task = task->next;
        }

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
        long now = millis();
        TASK *current = firstTask;
        TASK *next = NULL;

        while (current) 
            {
            next = current->next;
            if (current->millis > 0 && current->millis < now) 
                { // ToDo: handle overflow
                if (!executeTask(current)) 
                    {
                    deleteTask(current);
                    }
                }
            current = next;
            }
        }
    }

static bool executeTask(TASK *task)
    {
    // Find end of next command
    byte *msg = &task->data[task->currPos];
    byte *search = msg;
    int cmdSize;
    bool taskRescheduled = false;

    while (*search != HDLC_FRAME_FLAG && search != task->endData)
        {
        search++;
        }

    if ((cmdSize = search - msg - 1) != 0)
        {
        taskRescheduled = parseMessage(cmdSize, msg);  
        } 

    task->currPos += cmdSize + 1;
    if (task->currPos >= task->size)
        {
        if (taskRescheduled)
            {
            task->currPos = 0;
            return true;
            }
        else
            {
            return false;
            }
        }
    return true;
    }
