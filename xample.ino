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

bool ref0;

void haskinoMain()
    {
    ref0 = 0;
    pinMode(2,0);
    pinMode(6,1);
    pinMode(7,1);
    while (1)
        {
        bool bind0;
        bool bind1;

        bind0 = digitalRead(2);
        ref0 = bind0;
        bind1 = ref0;
        digitalWrite(6,bind1);
        digitalWrite(7,!(bind1));
        delayMilliseconds(100);
        }
    taskComplete();
    }

