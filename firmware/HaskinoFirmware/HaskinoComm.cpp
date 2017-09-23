#include <stdarg.h>
#include <Arduino.h>
#include <HardwareSerial.h>
#include "HaskinoAnalog.h"
#include "HaskinoBoardControl.h"
#include "HaskinoBoardStatus.h"
#include "HaskinoCodeBlock.h"
#include "HaskinoCommands.h"
#include "HaskinoComm.h"
#include "HaskinoConfig.h"
#include "HaskinoDigital.h"
#include "HaskinoExpr.h"
#include "HaskinoI2C.h"
#include "HaskinoOneWire.h"
#include "HaskinoRefs.h"
#include "HaskinoScheduler.h"
#include "HaskinoSerial.h"
#include "HaskinoServo.h"
#include "HaskinoStepper.h"

static int messageCount = 0;
static int processingEscapeState = 0;
static byte inputData[MESSAGE_MAX_SIZE];

static void processChar(byte c);

int processingMessage() 
    {
    return (messageCount != 0);
    }

bool parseMessage(int size, const byte *msg, CONTEXT *context)
    {
    switch (msg[0] & CMD_TYPE_MASK) 
        {
        case BC_CMD_TYPE:
            return parseBoardControlMessage(size, msg, context);
            break;
        case BS_CMD_TYPE:
            return parseBoardStatusMessage(size, msg, context);
            break;
#ifdef INCLUDE_DIG_CMDS
        case DIG_CMD_TYPE:
            return parseDigitalMessage(size, msg, context);
            break;
#endif
#ifdef INCLUDE_ALG_CMDS
        case ALG_CMD_TYPE:
            return parseAnalogMessage(size, msg, context);
            break;
#endif
#ifdef INCLUDE_I2C_CMDS
        case I2C_CMD_TYPE:
            return parseI2CMessage(size, msg, context);
            break;
#endif
#ifdef INCLUDE_ONEW_CMDS
        case ONEW_CMD_TYPE:
            return parseOneWireMessage(size, msg, context);
            break;
#endif
#ifdef INCLUDE_SRVO_CMDS
        case SRVO_CMD_TYPE:
            return parseServoMessage(size, msg, context);
            break;
#endif
#ifdef INCLUDE_STEP_CMDS
        case STEP_CMD_TYPE:
            return parseStepperMessage(size, msg, context);
            break;
#endif
#ifdef INCLUDE_SCHED_CMDS
        case SCHED_CMD_TYPE:
            return parseSchedulerMessage(size, msg, context);
            break;
#endif
#ifdef INCLUDE_SERIAL_CMDS
        case SER_CMD_TYPE:
            return parseSerialMessage(size, msg, context);
            break;
#endif
        case REF_CMD_TYPE:
            return parseRefMessage(size, msg, context);
            break;
        case EXPR_CMD_TYPE:
            return parseExprMessage(size, msg, context);
            break;
        }
        return false;
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
                parseMessage(messageCount-1, inputData, schedulerDefaultContext());
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

void sendReply(int count, byte replyType, const byte *reply, 
               CONTEXT *context, byte bind)
    {
    const byte *nextChar = reply;
    int i;

    if ((replyType != BS_RESP_STRING) && (context->currBlockLevel >= 0))
        {
        memcpy(&context->bind[bind * BIND_SPACING], reply, count);
        }
    else
        {
        startReplyFrame(replyType);
        for (i=0; i < count; i++) 
            {
            sendReplyByte(*nextChar++);
            }
        endReplyFrame();
        }
    }

void sendStringf(const char *fmt, ...)
    {
    char buffer[128];    
    va_list argp = NULL;

    va_start(argp, fmt);
    vsprintf(buffer, fmt, argp);
    va_end(argp);
    sendReply(strlen(buffer), BS_RESP_STRING, (const byte *) buffer, NULL, 0);
    }
