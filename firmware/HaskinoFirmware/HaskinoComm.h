#ifndef HaskinoCommH
#define HaskinoCommH

#define HDLC_FRAME_FLAG  0x7E
#define HDLC_ESCAPE      0x7D
#define HDLC_MASK        0x20

int  processingMessage();
void handleInput();
void startReplyFrame(byte replyType);
void endReplyFrame();
void sendReplyByte(byte replyByte);
void sendReply(int count, byte replyType, const byte *reply, 
               byte *local, byte bind);
void sendStringf(const char *fmt, ...);
bool parseMessage(int size, const byte *msg, byte *local);

#endif /* HaskinoCommH */
