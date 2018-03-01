#include <stdarg.h>
#include <Arduino.h>
#include <HardwareSerial.h>
#include "HaskinoAnalog.h"
#include "HaskinoBoardControl.h"
#include "HaskinoBoardStatus.h"
#include "HaskinoCommands.h"
#include "HaskinoComm.h"
#include "HaskinoConfig.h"
#include "HaskinoDigital.h"
#include "HaskinoI2C.h"
#include "HaskinoSerial.h"
#include "HaskinoServo.h"
#include "HaskinoStepper.h"

static int messageCount = 0;
static int processingEscapeState = 0;
static byte inputData[MESSAGE_MAX_SIZE];

static void processChar(byte c);

void parseMessage(int size, const byte *msg)
    {
    switch (msg[0] & CMD_TYPE_MASK) 
        {
        case BC_CMD_TYPE:
            parseBoardControlMessage(size, msg);
            break;
        case BS_CMD_TYPE:
            parseBoardStatusMessage(size, msg);
            break;
#ifdef INCLUDE_DIG_CMDS
        case DIG_CMD_TYPE:
            parseDigitalMessage(size, msg);
            break;
#endif
#ifdef INCLUDE_ALG_CMDS
        case ALG_CMD_TYPE:
            parseAnalogMessage(size, msg);
            break;
#endif
#ifdef INCLUDE_I2C_CMDS
        case I2C_CMD_TYPE:
            parseI2CMessage(size, msg);
            break;
#endif
#ifdef INCLUDE_SRVO_CMDS
        case SRVO_CMD_TYPE:
            parseServoMessage(size, msg);
            break;
#endif
#ifdef INCLUDE_STEP_CMDS
        case STEP_CMD_TYPE:
            parseStepperMessage(size, msg);
            break;
#endif
#ifdef INCLUDE_SERIAL_CMDS
        case SER_CMD_TYPE:
            parseSerialMessage(size, msg);
            break;
#endif
        }
    }

static void processChar(byte c)
    {
    if (c == HDLC_FRAME_FLAG) 
        {
        if (messageCount > 1)
            {
            byte checksum = 0;
            byte *msg = inputData;
            for (int i=0;i<messageCount-1;i++)
                {
                checksum += *msg++;
                }
            if (checksum == *msg)
                {
                parseMessage(messageCount-1, inputData);
                }
            }
        processingEscapeState = 0;
        messageCount = 0;
        } 
    else if (c == HDLC_ESCAPE) 
        {
        processingEscapeState = 1;
        } 
    else if (processingEscapeState) 
        {
        processingEscapeState = 0;
        inputData[messageCount++] = c ^ HDLC_MASK;
        }
    else
        {
        inputData[messageCount++] = c;
        }
    }

void handleInput()
    {
    int input = Serial.read();
    if (input != -1) 
        {
        processChar((byte) input);
        }  
    }

static byte outgoingChecksum;

void startReplyFrame(byte replyType)
    {
    Serial.write(replyType);
    outgoingChecksum = replyType;
    }

void endReplyFrame()
    {
    sendReplyByte(outgoingChecksum);
    Serial.write(HDLC_FRAME_FLAG);
    }

void sendReplyByte(byte replyByte)
    {
    if (replyByte == HDLC_FRAME_FLAG || 
        replyByte == HDLC_ESCAPE) 
        {
        Serial.write(HDLC_ESCAPE);
        Serial.write(replyByte ^ HDLC_MASK);
        }
    else
        {
        Serial.write(replyByte);
        }
    outgoingChecksum += replyByte;
    }

void sendReply(int count, byte replyType, const byte *reply)
    {
    const byte *nextChar = reply;
    int i;

    startReplyFrame(replyType);
    for (i=0; i < count; i++) 
        {
        sendReplyByte(*nextChar++);
        }
    endReplyFrame();
    }

void sendStringf(const char *fmt, ...)
    {
    char buffer[128];    
    va_list argp = NULL;

    va_start(argp, fmt);
    vsprintf(buffer, fmt, argp);
    va_end(argp);
    sendReply(strlen(buffer), BS_RESP_STRING, (const byte *) buffer);
    }
