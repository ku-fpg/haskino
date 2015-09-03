#ifndef HaskinoRefsH
#define HaskinoRefsH

bool parseRefMessage(int size, const byte *msg, byte *local);
bool readRefBool(int refIndex);
uint8_t readRefWord8(int refIndex);
uint16_t readRefWord16(int refIndex);
uint32_t readRefWord32(int refIndex);

#endif /* HaskinoRefsH */
