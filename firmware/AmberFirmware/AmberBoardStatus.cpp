#include <Arduino.h>
#include "AmberBoardStatus.h"
#include "AmberComm.h"
#include "AmberCommands.h"
#include "AmberFirmware.h"

static int handleRequestVersion(int size, byte *msg);
static int handleRequestType(int size, byte *msg);
static int handleRequestMicros(int size, byte *msg);
static int handleRequestMillis(int size, byte *msg);

int parseBoardStatusMessage(int size, byte *msg)
    {
    switch (msg[0] ) 
        {
        case BS_CMD_REQUEST_VERSION:
            return handleRequestVersion(size, msg);
            break;
        case BS_CMD_REQUEST_TYPE:
            return handleRequestType(size, msg);
            break;
        case BS_CMD_REQUEST_MICROS:
            return handleRequestMicros(size, msg);
            break;
        case BS_CMD_REQUEST_MILLIS:
            return handleRequestMillis(size, msg);
            break;
        }
    }

static int handleRequestVersion(int size, byte *msg)
    {
    static byte versionReply[2] = {FIRMWARE_MAJOR,
                                   FIRMWARE_MINOR};
        
    sendReply(sizeof(versionReply), BS_RESP_VERSION, versionReply);
    return 1;
    }

static int handleRequestType(int size, byte *msg)
    {
    static byte typeReply[1] = {
#if defined(__AVR_ATmega8__)
                                ATmega8_TYPE};
#elif defined(__AVR_ATmega168__)
                                ATmega168_TYPE};
#elif defined(__AVR_ATmega328P__)
                                ATmega328P_TYPE};
#elif defined(__AVR_ATmega1280__)
                                ATmega1280_TYPE};
#elif defined(__AVR_ATmega2560__)
                                ATmega256_TYPE};
#elif defined(__AVR_ATmega32U4__)
                                ATmega32U4_TYPE};
#elif defined(__AVR_ATmega644P__)
                                ATmega644P_TYPE};
#elif defined(__AVR_ATmega644__)
                                ATmega644_TYPE};
#elif defined(__AVR_ATmega645__)
                                ATmega645_TYPE};
#elif defined(__SAM3X8E__)
                                SAM3X8E_TYPE};
#elif defined(ARDUINO_LINUX)
                                X86_TYPE};
#else
#error "Please define a new processor type board"
#endif
    sendReply(sizeof(typeReply), BS_RESP_TYPE, typeReply);
    return 1;
    }

static int handleRequestMicros(int size, byte *msg)
    {
    uint32_t microReply = micros();

    sendReply(sizeof(uint32_t), BS_RESP_MICROS, (byte *) &microReply);
    return 1;
    }

static int handleRequestMillis(int size, byte *msg)
    {
    uint32_t milliReply = millis();

    sendReply(sizeof(uint32_t), BS_RESP_MILLIS, (byte *) &milliReply);
    return 1;
    }
