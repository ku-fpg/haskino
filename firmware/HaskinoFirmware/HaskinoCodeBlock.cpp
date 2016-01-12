#include <Arduino.h>
#include "HaskinoComm.h"
#include "HaskinoConfig.h"

static bool codeBlock = false;

void runCodeBlock(int blockSize, const byte * block, CONTEXT *context)
    {
    int currPos = 0;
    codeBlock = true;

    while (currPos < blockSize)
        {
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

        parseMessage(cmdSize, cmd, context);  

        currPos += cmdSize + 1;
        }
    codeBlock = false;
    }

bool isCodeBlock()
    {
    return codeBlock;
    }

