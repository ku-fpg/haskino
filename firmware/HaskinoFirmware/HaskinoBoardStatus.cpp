#include <Arduino.h>
#include "HaskinoBoardStatus.h"
#include "HaskinoComm.h"
#include "HaskinoCommands.h"
#include "HaskinoExpr.h"
#include "HaskinoFirmware.h"

static bool handleRequestVersion(int size, const byte *msg, byte *local);
static bool handleRequestType(int size, const byte *msg, byte *local);
static bool handleRequestMicros(int size, const byte *msg, byte *local);
static bool handleRequestMillis(int size, const byte *msg, byte *local);

bool parseBoardStatusMessage(int size, const byte *msg, byte *local)
    {
    switch (msg[0] ) 
        {
        case BS_CMD_REQUEST_VERSION:
            return handleRequestVersion(size, msg, local);
            break;
        case BS_CMD_REQUEST_TYPE:
            return handleRequestType(size, msg, local);
            break;
        case BS_CMD_REQUEST_MICROS:
            return handleRequestMicros(size, msg, local);
            break;
        case BS_CMD_REQUEST_MILLIS:
            return handleRequestMillis(size, msg, local);
            break;
        }
    return false;
    }

void sendVersionReply(byte *local)
    {
    static byte versionReply[2] = {FIRMWARE_MINOR,
                                   FIRMWARE_MAJOR};
        
    sendReply(sizeof(versionReply), BS_RESP_VERSION, versionReply, local);
    }

static bool handleRequestVersion(int size, const byte *msg, byte *local)
    {
    sendVersionReply(local);
    return false;
    }

static bool handleRequestType(int size, const byte *msg, byte *local)
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
#elif defined(INTEL_EDISON)
                                QUARK_TYPE};
#else
#error "Please define a new processor type board"
#endif
    sendReply(sizeof(typeReply), BS_RESP_TYPE, typeReply, local);
    return false;
    }

static bool handleRequestMicros(int size, const byte *msg, byte *local)
    {
    uint32_t ms;
    byte microReply[5];

    microReply[0] = EXPR(EXPR_WORD32, EXPR_LIT);
    ms = micros();
    memcpy(&microReply[1], &ms, sizeof(ms));

    sendReply(sizeof(microReply), BS_RESP_MICROS, (byte *) &microReply, local);
    return false;
    }

static bool handleRequestMillis(int size, const byte *msg, byte *local)
    {
    uint32_t ms;
    byte milliReply[5];

    milliReply[0] = EXPR(EXPR_WORD32, EXPR_LIT);
    ms = millis();
    memcpy(&milliReply[1], &ms, sizeof(ms));

    sendReply(sizeof(uint32_t), BS_RESP_MILLIS, (byte *) &milliReply, local);
    return false;
    }
