#include <Arduino.h>
#include "HaskinoComm.h"
#include "HaskinoConfig.h"

#undef  DEBUG

bool runCodeBlock(int blockSize, const byte * block, CONTEXT *context)
    {
    int currPos = 0;
    TASK *task = context->task;
    bool taskRescheduled;
    int16_t thisBlockLevel;
#ifdef DEBUG
    sendStringf("Run Block %d %d %d",task->rescheduled,context->recallBlockLevel,context->currBlockLevel);
#endif

    if (task && task->rescheduled)
        {
        // A task is running, and has been rescheduled.
        // Increase the block level as we return to the level at which we
        // rescheduled.
        context->recallBlockLevel++;
        // Go to the position in the block at which we were interrupted.
        currPos = context->blockStatus[context->recallBlockLevel].currPos;
        // Set the block level we are executing at.
        thisBlockLevel = context->recallBlockLevel;
        // If we have reached the level at which we were rescheduled, then
        // the rescheduling process is complete, and we can resume normal
        // operation.
        if (context->recallBlockLevel == context->currBlockLevel)
            {
            task->rescheduled = false;
            }
        }
    else
        {
        // Increase the current context block level
        context->currBlockLevel++;
        // Start execution at the start of the code block
        context->blockStatus[context->currBlockLevel].currPos = 0;
        thisBlockLevel = context->currBlockLevel;
        }
#ifdef DEBUG
    sendStringf("Run Block Lvl %d",thisBlockLevel);
#endif

    while (currPos < blockSize)
        {
#ifdef DEBUG
        sendStringf("Block %d",currPos);
#endif
        const byte *msg = &block[currPos];
        byte cmdSize;
        const byte *cmd;

        if (msg[0] != 0xFF)
            {
            cmdSize = msg[0];
            cmd = &msg[1];
            }
        else
            {
            cmdSize = ((uint16_t) msg[2]) << 8 |
                      ((uint16_t) msg[1]);
            cmd = &msg[3];
            }

        taskRescheduled = parseMessage(cmdSize, cmd, context); 

        if (!taskRescheduled || thisBlockLevel == context->currBlockLevel)
            {
            // If we weren't rescheduled, or we are running, then
            // move to the next command in the command block.
            currPos += cmdSize + 1;
            context->blockStatus[context->currBlockLevel].currPos = currPos;
            }
        if (task && taskRescheduled)
            {
            if (!task->rescheduled)
                {
                // Reset the recallBlockLevel for when task is reactivated. 
                context->recallBlockLevel = -1;
                task->rescheduled = true;
                }
#ifdef DEBUG
            sendStringf("Resched Exit Block Lvl %d",thisBlockLevel);
#endif
            return true;
            } 
        }

    context->currBlockLevel--;
#ifdef DEBUG
    sendStringf("Normal Exit Block Lvl %d",thisBlockLevel);
#endif
    return false;
    }
