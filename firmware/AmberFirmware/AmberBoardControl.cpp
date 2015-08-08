#include <Arduino.h>
#include "AmberBoardControl.h"
#include "AmberComm.h"
#include "AmberCommands.h"
#include "AmberFirmware.h"

static void handleRequestVersion(int size, char *msg);
static void handleSetPinMode(int size, char *msg);
static void handleRequestMicros(int size, char *msg);
static void handleRequestMillis(int size, char *msg);
static void handleDelayMillis(int size, char *msg);
static void handleDelayMicros(int size, char *msg);
static void handleSystemReset(int size, char *msg);

void parseBoardControlMessage(int size, char *msg)
    {
    switch (msg[0] ) 
        {
        case BC_CMD_REQUEST_VERSION:
            handleRequestVersion(size, msg);
            break;
        case BC_CMD_SET_PIN_MODE:
            handleSetPinMode(size, msg);
            break;
        case BC_CMD_REQUEST_MICROS:
            handleRequestMicros(size, msg);
            break;
        case BC_CMD_REQUEST_MILLIS:
            handleRequestMillis(size, msg);
            break;
        case BC_CMD_DELAY_MILLIS:
            handleDelayMillis(size, msg);
            break;
        case BC_CMD_DELAY_MICROS:
            handleDelayMicros(size, msg);
            break;
        case BC_CMD_SYSTEM_RESET:
            handleSystemReset(size, msg);
            break;
        }
    }

static void handleRequestVersion(int size, char *msg)
    {
    static char versionReply[3] = {BC_RESP_VERSION,
                                   FIRMWARE_MAJOR,
                                   FIRMWARE_MINOR};
        
    sendReply(sizeof(versionReply), versionReply);
    }

static void handleSetPinMode(int size, char *msg)
    {
        
    }

static void handleRequestMicros(int size, char *msg)
    {
        
    }

static void handleRequestMillis(int size, char *msg)
    {
        
    }

static void handleDelayMillis(int size, char *msg)
    {
        
    }

static void handleDelayMicros(int size, char *msg)
    {
        
    }

static void handleSystemReset(int size, char *msg)
    {
        
    }

void systemReset()
    {
#if 0 /* TBD */
    // initialize a defalt state

    // pins with analog capability default to analog input
    // otherwise, pins default to digital output
    for (byte i = 0; i < TOTAL_PINS; i++) 
        {
        if (IS_PIN_ANALOG(i)) 
            {
            pinMode(i, ANALOG);
            }
        else if (IS_PIN_DIGITAL(i)) 
            {
            pinMode(i, OUTPUT);
            }
        }
#endif
    }
