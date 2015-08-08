#include <Arduino.h>
#include <HardwareSerial.h>
#include "AmberAnalog.h"
#include "AmberBoardControl.h"
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
static char inputData[MESSAGE_MAX_SIZE];

static void parseMessage(int size, char *msg);
static void processChar(char c);

int processingMessage() 
    {
    return processingMessageState;
    }

static void parseMessage(int size, char *msg)
    {
    switch (msg[0] & CMD_TYPE_MASK) 
        {
        case BC_CMD_MASK:
            parseBoardControlMessage(size, msg);
            break;
        case DIG_CMD_MASK:
            parseDigitalMessage(size, msg);
            break;
        case ALG_CMD_MASK:
            parseAnalogMessage(size, msg);
            break;
        case I2C_CMD_MASK:
            parseI2CMessage(size, msg);
            break;
        case ONEW_CMD_MASK:
            parseOneWireMessage(size, msg);
            break;
        case SRVO_CMD_MASK:
            parseServoMessage(size, msg);
            break;
        case STEP_CMD_MASK:
            parseStepperMessage(size, msg);
            break;
        case SCHED_CMD_MASK:
            parseSchedulerMessage(size, msg);
            break;
        }
    }

static void processChar(char c)
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
        processChar((char) input);
        }  
    }

void sendReply(int count, char *reply)
    {
    char *nextChar = reply;
    int i;

    for (i=0; i < count; i++, nextChar++) 
        {
        if (*nextChar == HDLC_FRAME_FLAG || 
            *nextChar == HDLC_ESCAPE) 
            {
            Serial.write(HDLC_ESCAPE);
            Serial.write(*nextChar ^ HDLC_ESCAPE);
            }
        else
            {
            Serial.write(*nextChar);
            }
        }
    }
