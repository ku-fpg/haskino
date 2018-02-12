#include <Arduino.h>
#include "HaskinoBoardStatus.h"
#include "HaskinoComm.h"
#include "HaskinoCommands.h"
#include "HaskinoConfig.h"
#include "HaskinoExpr.h"
#include "HaskinoFirmware.h"

static bool handleRequestVersion(int size, const byte *msg, CONTEXT *context);
static bool handleRequestType(int size, const byte *msg, CONTEXT *context);
static bool handleRequestMicros(int size, const byte *msg, CONTEXT *context);
static bool handleRequestMillis(int size, const byte *msg, CONTEXT *context);
static bool handleDebug(int size, const byte *msg, CONTEXT *context);

bool parseBoardStatusMessage(int size, const byte *msg, CONTEXT *context)
    {
    switch (msg[0] ) 
        {
        case BS_CMD_REQUEST_VERSION:
            return handleRequestVersion(size, msg, context);
            break;
        case BS_CMD_REQUEST_TYPE:
            return handleRequestType(size, msg, context);
            break;
        case BS_CMD_REQUEST_MICROS:
            return handleRequestMicros(size, msg, context);
            break;
        case BS_CMD_REQUEST_MILLIS:
            return handleRequestMillis(size, msg, context);
            break;
        case BS_CMD_DEBUG:
            return handleDebug(size, msg, context);
            break;
        }
    return false;
    }

void sendVersionReply(CONTEXT *context, byte bind)
    {
    static byte versionReply[2] = {FIRMWARE_MINOR,
                                   FIRMWARE_MAJOR};
        
    sendReply(sizeof(versionReply), BS_RESP_VERSION, versionReply, context, bind);
    }

static bool handleRequestVersion(int size, const byte *msg, CONTEXT *context)
    {
    byte bind = msg[1];
    sendVersionReply(context, bind);
    return false;
    }

static bool handleRequestType(int size, const byte *msg, CONTEXT *context)
    {
    byte bind = msg[1];
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
    sendReply(sizeof(typeReply), BS_RESP_TYPE, typeReply, context, bind);
    return false;
    }

static bool handleRequestMicros(int size, const byte *msg, CONTEXT *context)
    {
    byte bind = msg[1];
    uint32_t ms;
    byte microReply[6];

    microReply[0] = EXPR_WORD32;
    microReply[1] = EXPR_LIT;
    ms = micros();
    memcpy(&microReply[2], &ms, sizeof(ms));

    sendReply(2 + sizeof(microReply), BS_RESP_MICROS, 
              (byte *) &microReply, context, bind);
    return false;
    }

static bool handleRequestMillis(int size, const byte *msg, CONTEXT *context)
    {
    byte bind = msg[1];
    uint32_t ms;
    byte milliReply[6];

    milliReply[0] = EXPR_WORD32;
    milliReply[1] = EXPR_LIT;
    ms = millis();
    memcpy(&milliReply[2], &ms, sizeof(ms));

    sendReply(2 + sizeof(uint32_t), BS_RESP_MILLIS, 
              (byte *) &milliReply, context, bind);
    return false;
    }

static bool handleDebug(int size, const byte *msg, CONTEXT *context)
    {
    bool alloc;
    byte *expr = (byte *) &msg[2];
    byte bind = msg[1];
    uint8_t *string = evalList8Expr(&expr, context, &alloc);

    /* Send the output of the debug with no context, so that it will
     * always go out the serial port, even if we are executing a code 
     * block */
    sendReply(string[2], BS_RESP_STRING, &string[3], NULL, 0);
    /* Send the debug reply so if we are not executing a code block
     * host will continue */
    sendReply(0, BS_RESP_DEBUG, NULL, context, bind);

    if (alloc)
        free(string);
    return false;
    }
