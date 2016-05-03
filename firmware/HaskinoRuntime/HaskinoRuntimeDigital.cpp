#include <Arduino.h>
#include "HaskinoRuntime.h"

// Digital port routines

static uint8_t bits[8] = {0x01,0x02,0x04,0x08,0x10,0x20,0x40,0x80};

void digitalPortWrite(uint8_t pinNo, uint8_t value, uint8_t mask)
    {
    for (int i=0;i<8;i++)
        {
        if (bits[i] & mask) 
            {
            digitalWrite(pinNo+i, (bits[i] & value) == bits[i]);
            }
        }
    }
    
uint8_t digitalPortRead(uint8_t pinNo, uint8_t mask)
    {
    uint8_t value = 0;

    for (int i=0;i<8;i++)
        {
        if ((bits[i] & mask) && digitalRead(pinNo+i))
            value |= bits[i];
        }

    return (value);
    }
    
