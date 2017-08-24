#include "HaskinoRuntime.h"

void haskinoMain();
#define HASKINOMAIN_STACK_SIZE 108
byte haskinoMainTcb[sizeof(TCB) + HASKINOMAIN_STACK_SIZE];

void setup()
    {
    haskinoMemInit();
    createTask(255, haskinoMainTcb, HASKINOMAIN_STACK_SIZE, haskinoMain);
    scheduleTask(255, 0);
    startScheduler();
    }

void loop()
    {
    }


void haskinoMain()
    {
    pinMode(13,1);
    while (1)
        {
        digitalWrite(13,1);
        delayMilliseconds(1000);
        digitalWrite(13,0);
        delayMilliseconds(1000);
        }
    taskComplete();
    }
