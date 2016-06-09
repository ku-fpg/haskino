/******************************************************************************
 *
 * Module      :  HaskinoRuntimeStatus
 * Copyright   :  (c) University of Kansas
 * License     :  BSD3
 * Stability   :  experimental
 *
 * Haskino Runtime Board Status module
 *****************************************************************************/

#include <Arduino.h>
#include "HaskinoRuntime.h"

// Platform Query routines

uint16_t queryFirmware()
    {
    return (FIRMWARE_MAJOR << 8) | FIRMWARE_MINOR;
    }

uint8_t queryProcessor()
    {
    static uint8_t type = 
#if defined(__AVR_ATmega8__)
                                ATmega8_TYPE;
#elif defined(__AVR_ATmega168__)
                                ATmega168_TYPE;
#elif defined(__AVR_ATmega328P__)
                                ATmega328P_TYPE;
#elif defined(__AVR_ATmega1280__)
                                ATmega1280_TYPE;
#elif defined(__AVR_ATmega2560__)
                                ATmega256_TYPE;
#elif defined(__AVR_ATmega32U4__)
                                ATmega32U4_TYPE;
#elif defined(__AVR_ATmega644P__)
                                ATmega644P_TYPE;
#elif defined(__AVR_ATmega644__)
                                ATmega644_TYPE;
#elif defined(__AVR_ATmega645__)
                                ATmega645_TYPE;
#elif defined(__SAM3X8E__)
                                SAM3X8E_TYPE;
#elif defined(ARDUINO_LINUX)
                                X86_TYPE;
#elif defined(INTEL_EDISON)
                                QUARK_TYPE;
#else
#error "Please define a new processor type board"
#endif

    return type;
    }
    
