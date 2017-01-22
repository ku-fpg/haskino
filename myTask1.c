#include "HaskinoRuntime.h"

void haskinoMain();
#define HASKINOMAIN_STACK_SIZE 104
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

uint8_t ref0;

void haskinoMain()
    {
    pinMode(13,1);
    ref0 = 0;
    while (1)
        {
        uint32_t bind0;

        takeSem(0);
        bind0 = 0
        while ((bind0 < 3))
            {
            digitalWrite(13,1);
            delayMilliseconds(125);
            digitalWrite(13,0);
            delayMilliseconds(125);
            bind0 = (bind0 + 1);
            }
        }
    taskComplete();
    }
