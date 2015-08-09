#ifndef AmberCommH
#define AmberCommH

#define MESSAGE_MAX_SIZE 128

int  processingMessage();
void handleInput();
void sendReply(int count, unsigned char *reply);

#endif /* AmberCommH */
