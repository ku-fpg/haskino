#include "HaskinoRuntime.h"

void haskinoMain();
void task1();

bool ref0;
uint8_t ref1;

void task1()
    {
    digitalWrite(2,1);
    digitalWrite(3,0);
    ref0 = 0;
    for (int i=0, bind2 = {4,1,2,3,4},
         bind1 = list8Elem(bind2, 0);
         i = list8Len(bind2);
         i++, bind1 = list8Elem(bind2, i))
        {
        ref1 = (ref1 + bind1);
        }
    }

void haskinoMain()
    {
    uint32_t bind1;

    ref0 = 1;
    createTask(1, task1());
    bind1 = millis();
    while (1)
        {
        uint32_t bind2;

        pinMode(2,0);
        pinMode(3,1);
        bind2 = millis();
        }
    }

