#include <Arduino.h>
#include <HardwareSerial.h>
#include "AmberAnalog.h"
#include "AmberBoardControl.h"
#include "AmberBoardStatus.h"
#include "AmberCommands.h"
#include "AmberComm.h"
#include "AmberDigital.h"
#include "AmberI2C.h"
#include "AmberOneWire.h"
#include "AmberScheduler.h"
#include "AmberServo.h"
#include "AmberStepper.h"

#define HDLC_FRAME_FLAG  0x7E
#define HDLC_ESCAPE      0x7D
#define HDLC_MASK        0x20

static int messageCount = 0;
static int processingMessageState = 0;
static int processingEscapeState = 0;
static byte inputData[MESSAGE_MAX_SIZE];

static int parseMessage(int size, byte *msg);
static void processChar(char c);

int processingMessage() 
    {
    return processingMessageState;
    }

static int parseMessage(int size, byte *msg)
    {
    switch (msg[0] & CMD_TYPE_MASK) 
        {
        case BC_CMD_TYPE:
            return parseBoardControlMessage(size, msg);
            break;
        case BS_CMD_TYPE:
            return parseBoardStatusMessage(size, msg);
            break;
        case DIG_CMD_TYPE:
            return parseDigitalMessage(size, msg);
            break;
        case ALG_CMD_TYPE:
            return parseAnalogMessage(size, msg);
            break;
        case I2C_CMD_TYPE:
            return parseI2CMessage(size, msg);
            break;
        case ONEW_CMD_TYPE:
            return parseOneWireMessage(size, msg);
            break;
        case SRVO_CMD_TYPE:
            return parseServoMessage(size, msg);
            break;
        case STEP_CMD_TYPE:
            return parseStepperMessage(size, msg);
            break;
        case SCHED_CMD_TYPE:
            return parseSchedulerMessage(size, msg);
            break;
        }
    }

static void processChar(byte c)
    {
    if (processingMessageState) 
        {
        if (c == HDLC_FRAME_FLAG) 
            {
            processingMessageState = 0;
            processingEscapeState = 0;
            parseMessage(messageCount, inputData);
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
    else if (c == HDLC_FRAME_FLAG) 
        {
        processingMessageState = 1;
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

void startReplyFrame(byte replyType)
    {
    Serial.write(HDLC_FRAME_FLAG);
    Serial.write(replyType);
    }

void endReplyFrame()
    {
    Serial.write(HDLC_FRAME_FLAG);
    }

void sendReplyByte(byte replyByte)
    {
    if (replyByte == HDLC_FRAME_FLAG || 
        replyByte == HDLC_ESCAPE) 
        {
        Serial.write(HDLC_ESCAPE);
        Serial.write(replyByte ^ HDLC_ESCAPE);
        }
    else
        {
        Serial.write(replyByte);
        }
    }

void sendReply(int count, byte replyType, byte *reply)
    {
    byte *nextChar = reply;
    int i;

    startReplyFrame(replyType);
    for (i=0; i < count; i++) 
        {
        sendReplyByte(*nextChar++);
        }
    endReplyFrame();
    }

void sendString(char *reply)
    {
    sendReply(strlen(reply), BS_RESP_STRING, (byte *) reply);
    }
