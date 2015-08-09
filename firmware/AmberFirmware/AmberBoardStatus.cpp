#include <Arduino.h>
#include "AmberBoardStatus.h"
#include "AmberComm.h"
#include "AmberCommands.h"
#include "AmberFirmware.h"

static void handleRequestVersion(int size, unsigned char *msg);
static void handleRequestMicros(int size, unsigned char *msg);
static void handleRequestMillis(int size, unsigned char *msg);

void parseBoardStatusMessage(int size, unsigned char *msg)
    {
    switch (msg[0] ) 
        {
        case BS_CMD_REQUEST_VERSION:
            handleRequestVersion(size, msg);
            break;
        case BS_CMD_REQUEST_MICROS:
            handleRequestMicros(size, msg);
            break;
        case BS_CMD_REQUEST_MILLIS:
            handleRequestMillis(size, msg);
            break;
        }
    }

static void handleRequestVersion(int size, unsigned char *msg)
    {
    static unsigned char versionReply[3] = {BS_RESP_VERSION,
                                            FIRMWARE_MAJOR,
                                            FIRMWARE_MINOR};
        
    sendReply(sizeof(versionReply), versionReply);
    }

static void handleRequestMicros(int size, unsigned char *msg)
    {
    unsigned char microReply[5];
    unsigned long m;

    microReply[0] = BS_RESP_MICROS;
    m = micros();
    memcpy(&microReply[1], &m, 4);
    sendReply(sizeof(microReply), microReply);
    }

static void handleRequestMillis(int size, unsigned char *msg)
    {
    unsigned char milliReply[5];
    unsigned long m;

    milliReply[0] = BS_RESP_MILLIS;
    m = millis();
    memcpy(&milliReply[1], &m, 4);
    sendReply(sizeof(milliReply), milliReply);
    }
