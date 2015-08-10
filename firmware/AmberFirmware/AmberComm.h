#ifndef AmberCommH
#define AmberCommH

#define MESSAGE_MAX_SIZE 128

int  processingMessage();
void handleInput();
void sendReply(int count, byte replyType, byte *reply);
void sendString(char *string);

#endif /* AmberCommH */
