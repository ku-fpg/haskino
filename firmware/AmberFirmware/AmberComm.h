#ifndef AmberCommH
#define AmberCommH

#define MESSAGE_MAX_SIZE 128

int  processingMessage();
void handleInput();
void startReplyFrame(byte replyType);
void endReplyFrame();
void sendReplyByte(byte replyByte);
void sendReply(int count, byte replyType, byte *reply);
void sendString(char *string);

#endif /* AmberCommH */
