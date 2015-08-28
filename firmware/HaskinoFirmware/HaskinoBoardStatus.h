#ifndef HaskinoBoardStatusH
#define HaskinoBoardStatusH

#define ATmega8_TYPE        0
#define ATmega168_TYPE      1
#define ATmega328P_TYPE     2
#define ATmega1280_TYPE     2
#define ATmega256_TYPE      4
#define ATmega32U4_TYPE     5
#define ATmega644P_TYPE     6
#define ATmega644_TYPE      7
#define ATmega645_TYPE      8
#define SAM3X8E_TYPE        9
#define X86_TYPE            10

bool parseBoardStatusMessage(int size, byte *msg, byte *local);
void sendVersionReply(byte *local);

#endif /* HaskinoBoardStatusH */
