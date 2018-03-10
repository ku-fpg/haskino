#include "HaskinoRuntime.h"

void haskinoMain();
#define HASKINOMAIN_STACK_SIZE 920
byte haskinoMainTcb[sizeof(TCB) + HASKINOMAIN_STACK_SIZE];

// ******** Start of Instrumentation            
#define TEST_COUNT 1000
#define TEST_CMD  0x20
static uint8_t cmdChar;
static uint32_t start;
static uint32_t end;
static uint32_t cmdCount = 0;
static uint32_t charCount;

void sendStringf(const char *fmt, ...);

// ******** End of Instrumentation            

void setup()
    {
    haskinoMemInit();
    createTask(255, haskinoMainTcb, HASKINOMAIN_STACK_SIZE, haskinoMain);
    scheduleTask(255, 0);
    startScheduler();
    }

void loop()
    {
    }

uint8_t ref0;

void haskinoMain()
    {
    int32_t bind0;

    ref0 = 0;
    serialBegin(0,115200);
    bind0 = 0;
    int32_t bind3;
    uint8_t * bind4 = NULL;
    uint8_t * bind5 = NULL;
    while (1)
        {
        ref0 = 0;
        listAssign(&bind4, (uint8_t * ) (const byte[]) {255, 0});
        bind3 = 0;
        int32_t bind6;
        uint8_t bind8;
        // ******** Start of Instrumentation
        charCount = 0;
        // ******** End of Instrumentation
        while (1)
            {
            bind6 = 0;
            uint32_t bind9;
            while (1)
                {
                bind9 = serialAvailable(0);
                if ((0 < bind9))
                    {
                    int32_t bind10;

                    bind10 = serialRead(0);
                    bind8 = ((uint8_t) ((int32_t) bind10));
                    break;
                    }
                else
                    {
                    bind6 = 0;
                    }
                }
            if ((bind8 == 125))
                {
                int32_t bind11;
                uint8_t bind13;
                uint8_t bind16;

                bind11 = 0;
                uint32_t bind14;
                while (1)
                    {
                    bind14 = serialAvailable(0);
                    if ((0 < bind14))
                        {
                        int32_t bind15;

                        bind15 = serialRead(0);
                        bind13 = ((uint8_t) ((int32_t) bind15));
                        break;
                        }
                    else
                        {
                        bind11 = 0;
                        }
                    }
                bind16 = ref0;
                ref0 = ((bind16 + bind13) ^ 32);
                }
            else
                {
                if ((bind8 == 126))
                    {
                    uint8_t bind17;
                    uint8_t * bind18 = NULL;

                    bind17 = ref0;
                    if ((((uint8_t) (bind17 - list8Elem(bind4,(list8Len(bind4) - 1)))) == ((uint8_t) list8Elem(bind4,(list8Len(bind4) - 1)))))
                        {
                        listAssign(&bind18, list8Slice(bind4,0,(list8Len(bind4) - 1)));
                        }
                    else
                        {
                        listAssign(&bind18, (uint8_t * ) (const byte[]) {255, 0});
                        }
                    listAssign(&bind5, bind18);
                    listRelease(&bind18);
                    break;
                    }
                else
                    {
                    uint8_t bind19;

                    // ******** Start of Instrumentation
                    if (charCount == 0 && bind8 == TEST_CMD && cmdCount == 0)
                        {
                        cmdChar = bind8;
                        start = millis();
                        }
                    if (charCount == 0 && bind8 == TEST_CMD)
                        {
                        cmdCount++;
                        }
                    charCount++;
                    // ******** End of Instrumentation
                    bind19 = ref0;
                    ref0 = (bind19 + bind8);
                    bind3 = 0;
                    listAssign(&bind4, list8Apnd(bind4,list8Cons(bind8,(uint8_t * ) (const byte[]) {255, 0})));
                    }
                }
            }
        if ((list8Len(bind5) == 0))
            {
            }
        else
            {
            if ((((uint8_t) (list8Elem(bind5,0) & 240)) == 16))
                {
                if ((((uint8_t) list8Elem(bind5,0)) == 16))
                    {
                    soft_restart();
                    }
                else
                    {
                    if ((((uint8_t) list8Elem(bind5,0)) == 17))
                        {
                        if (((((uint8_t) list8Elem(bind5,(1 + 0))) == 2) && ((((uint8_t) list8Elem(bind5,(1 + 1))) == 0) && ((((uint8_t) list8Elem(bind5,(1 + 3))) == 2) && (((uint8_t) list8Elem(bind5,(1 + 4))) == 0)))))
                            {
                            pinMode(list8Elem(bind5,(1 + 2)),(((uint8_t) list8Elem(bind5,(1 + 5))) == 0) ? INPUT : (((uint8_t) list8Elem(bind5,(1 + 5))) == 1) ? OUTPUT : (((uint8_t) list8Elem(bind5,(1 + 5))) == 2) ? INPUT_PULLUP : INPUT);
                            }
                        else
                            {
                            }
                        }
                    else
                        {
                        if ((((uint8_t) list8Elem(bind5,0)) == 18))
                            {
                            if (((((uint8_t) list8Elem(bind5,(1 + 1))) == 4) && (((uint8_t) list8Elem(bind5,(1 + 2))) == 0)))
                                {
                                int32_t bind27;
                                uint8_t * bind28 = NULL;

                                delayMilliseconds(((((((uint32_t) ((int32_t) list8Elem(bind5,(1 + 6)))) << 24) | (((uint32_t) ((int32_t) list8Elem(bind5,(1 + 5)))) << 16)) | (((uint32_t) ((int32_t) list8Elem(bind5,(1 + 4)))) << 8)) | ((uint32_t) ((int32_t) list8Elem(bind5,(1 + 3))))));
                                listAssign(&bind28, list8Cons(0,list8Cons(24,(uint8_t * ) (const byte[]) {255, 0})));
                                bind27 = 0;
                                while (1)
                                    {
                                    if (((((uint8_t) list8Elem(bind28,1)) == 126) || (((uint8_t) list8Elem(bind28,1)) == 125)))
                                        {
                                        serialWrite(0,125);
                                        serialWrite(0,(list8Elem(bind28,1) ^ 32));
                                        }
                                    else
                                        {
                                        serialWrite(0,list8Elem(bind28,1));
                                        }
                                    if ((list8Len(bind28) == 2))
                                        {
                                        if (((((uint8_t) (list8Elem(bind28,0) + list8Elem(bind28,1))) == 126) || (((uint8_t) (list8Elem(bind28,0) + list8Elem(bind28,1))) == 125)))
                                            {
                                            serialWrite(0,125);
                                            serialWrite(0,((list8Elem(bind28,0) + list8Elem(bind28,1)) ^ 32));
                                            }
                                        else
                                            {
                                            serialWrite(0,(list8Elem(bind28,0) + list8Elem(bind28,1)));
                                            }
                                        break;
                                        }
                                    else
                                        {
                                        bind27 = 0;
                                        listAssign(&bind28, list8Cons((list8Elem(bind28,0) + list8Elem(bind28,1)),list8Slice(bind28,2,0)));
                                        }
                                    }
                                serialWrite(0,126);
                                listRelease(&bind28);
                                }
                            else
                                {
                                }
                            }
                        else
                            {
                            if ((((uint8_t) list8Elem(bind5,0)) == 19))
                                {
                                if (((((uint8_t) list8Elem(bind5,(1 + 1))) == 4) && (((uint8_t) list8Elem(bind5,(1 + 2))) == 0)))
                                    {
                                    int32_t bind34;
                                    uint8_t * bind35 = NULL;

                                    delayMicroseconds(((((((uint32_t) ((int32_t) list8Elem(bind5,(1 + 6)))) << 24) | (((uint32_t) ((int32_t) list8Elem(bind5,(1 + 5)))) << 16)) | (((uint32_t) ((int32_t) list8Elem(bind5,(1 + 4)))) << 8)) | ((uint32_t) ((int32_t) list8Elem(bind5,(1 + 3))))));
                                    listAssign(&bind35, list8Cons(0,list8Cons(24,(uint8_t * ) (const byte[]) {255, 0})));
                                    bind34 = 0;
                                    while (1)
                                        {
                                        if (((((uint8_t) list8Elem(bind35,1)) == 126) || (((uint8_t) list8Elem(bind35,1)) == 125)))
                                            {
                                            serialWrite(0,125);
                                            serialWrite(0,(list8Elem(bind35,1) ^ 32));
                                            }
                                        else
                                            {
                                            serialWrite(0,list8Elem(bind35,1));
                                            }
                                        if ((list8Len(bind35) == 2))
                                            {
                                            if (((((uint8_t) (list8Elem(bind35,0) + list8Elem(bind35,1))) == 126) || (((uint8_t) (list8Elem(bind35,0) + list8Elem(bind35,1))) == 125)))
                                                {
                                                serialWrite(0,125);
                                                serialWrite(0,((list8Elem(bind35,0) + list8Elem(bind35,1)) ^ 32));
                                                }
                                            else
                                                {
                                                serialWrite(0,(list8Elem(bind35,0) + list8Elem(bind35,1)));
                                                }
                                            break;
                                            }
                                        else
                                            {
                                            bind34 = 0;
                                            listAssign(&bind35, list8Cons((list8Elem(bind35,0) + list8Elem(bind35,1)),list8Slice(bind35,2,0)));
                                            }
                                        }
                                    serialWrite(0,126);
                                    listRelease(&bind35);
                                    }
                                else
                                    {
                                    }
                                }
                            else
                                {
                                }
                            }
                        }
                    }
                }
            else
                {
                if ((((uint8_t) (list8Elem(bind5,0) & 240)) == 32))
                    {
                    if ((((uint8_t) list8Elem(bind5,0)) == 32))
                        {
                        uint16_t bind41;
                        int32_t bind42;
                        uint8_t * bind43 = NULL;

                        bind41 = queryFirmware();
                        listAssign(&bind43, list8Cons(0,list8Cons(40,list8Cons(((uint8_t) ((int32_t) (bind41 & 255))),list8Cons(((uint8_t) ((int32_t) (bind41 >> 8))),(uint8_t * ) (const byte[]) {255, 0})))));
                        bind42 = 0;
                        while (1)
                            {
                            if (((((uint8_t) list8Elem(bind43,1)) == 126) || (((uint8_t) list8Elem(bind43,1)) == 125)))
                                {
                                serialWrite(0,125);
                                serialWrite(0,(list8Elem(bind43,1) ^ 32));
                                }
                            else
                                {
                                serialWrite(0,list8Elem(bind43,1));
                                }
                            if ((list8Len(bind43) == 2))
                                {
                                if (((((uint8_t) (list8Elem(bind43,0) + list8Elem(bind43,1))) == 126) || (((uint8_t) (list8Elem(bind43,0) + list8Elem(bind43,1))) == 125)))
                                    {
                                    serialWrite(0,125);
                                    serialWrite(0,((list8Elem(bind43,0) + list8Elem(bind43,1)) ^ 32));
                                    }
                                else
                                    {
                                    serialWrite(0,(list8Elem(bind43,0) + list8Elem(bind43,1)));
                                    }
                                break;
                                }
                            else
                                {
                                bind42 = 0;
                                listAssign(&bind43, list8Cons((list8Elem(bind43,0) + list8Elem(bind43,1)),list8Slice(bind43,2,0)));
                                }
                            }
                        serialWrite(0,126);
                        listRelease(&bind43);
                        }
                    else
                        {
                        if ((((uint8_t) list8Elem(bind5,0)) == 33))
                            {
                            uint8_t bind48;
                            int32_t bind49;
                            uint8_t * bind50 = NULL;

                            bind48 = queryProcessor();
                            listAssign(&bind50, list8Cons(0,list8Cons(41,list8Cons(bind48,(uint8_t * ) (const byte[]) {255, 0}))));
                            bind49 = 0;
                            while (1)
                                {
                                if (((((uint8_t) list8Elem(bind50,1)) == 126) || (((uint8_t) list8Elem(bind50,1)) == 125)))
                                    {
                                    serialWrite(0,125);
                                    serialWrite(0,(list8Elem(bind50,1) ^ 32));
                                    }
                                else
                                    {
                                    serialWrite(0,list8Elem(bind50,1));
                                    }
                                if ((list8Len(bind50) == 2))
                                    {
                                    if (((((uint8_t) (list8Elem(bind50,0) + list8Elem(bind50,1))) == 126) || (((uint8_t) (list8Elem(bind50,0) + list8Elem(bind50,1))) == 125)))
                                        {
                                        serialWrite(0,125);
                                        serialWrite(0,((list8Elem(bind50,0) + list8Elem(bind50,1)) ^ 32));
                                        }
                                    else
                                        {
                                        serialWrite(0,(list8Elem(bind50,0) + list8Elem(bind50,1)));
                                        }
                                    break;
                                    }
                                else
                                    {
                                    bind49 = 0;
                                    listAssign(&bind50, list8Cons((list8Elem(bind50,0) + list8Elem(bind50,1)),list8Slice(bind50,2,0)));
                                    }
                                }
                            serialWrite(0,126);
                            listRelease(&bind50);
                            }
                        else
                            {
                            if ((((uint8_t) list8Elem(bind5,0)) == 34))
                                {
                                uint32_t bind55;
                                int32_t bind56;
                                uint8_t * bind57 = NULL;

                                bind55 = micros();
                                listAssign(&bind57, list8Cons(0,list8Cons(42,list8Cons(4,list8Cons(0,list8Cons(((uint8_t) ((int32_t) (bind55 & 255))),list8Cons(((uint8_t) ((int32_t) (bind55 >> 8))),list8Cons(((uint8_t) ((int32_t) (bind55 >> 16))),list8Cons(((uint8_t) ((int32_t) (bind55 >> 24))),(uint8_t * ) (const byte[]) {255, 0})))))))));
                                bind56 = 0;
                                while (1)
                                    {
                                    if (((((uint8_t) list8Elem(bind57,1)) == 126) || (((uint8_t) list8Elem(bind57,1)) == 125)))
                                        {
                                        serialWrite(0,125);
                                        serialWrite(0,(list8Elem(bind57,1) ^ 32));
                                        }
                                    else
                                        {
                                        serialWrite(0,list8Elem(bind57,1));
                                        }
                                    if ((list8Len(bind57) == 2))
                                        {
                                        if (((((uint8_t) (list8Elem(bind57,0) + list8Elem(bind57,1))) == 126) || (((uint8_t) (list8Elem(bind57,0) + list8Elem(bind57,1))) == 125)))
                                            {
                                            serialWrite(0,125);
                                            serialWrite(0,((list8Elem(bind57,0) + list8Elem(bind57,1)) ^ 32));
                                            }
                                        else
                                            {
                                            serialWrite(0,(list8Elem(bind57,0) + list8Elem(bind57,1)));
                                            }
                                        break;
                                        }
                                    else
                                        {
                                        bind56 = 0;
                                        listAssign(&bind57, list8Cons((list8Elem(bind57,0) + list8Elem(bind57,1)),list8Slice(bind57,2,0)));
                                        }
                                    }
                                serialWrite(0,126);
                                listRelease(&bind57);
                                }
                            else
                                {
                                if ((((uint8_t) list8Elem(bind5,0)) == 35))
                                    {
                                    uint32_t bind62;
                                    int32_t bind63;
                                    uint8_t * bind64 = NULL;

                                    bind62 = millis();
                                    listAssign(&bind64, list8Cons(0,list8Cons(43,list8Cons(4,list8Cons(0,list8Cons(((uint8_t) ((int32_t) (bind62 & 255))),list8Cons(((uint8_t) ((int32_t) (bind62 >> 8))),list8Cons(((uint8_t) ((int32_t) (bind62 >> 16))),list8Cons(((uint8_t) ((int32_t) (bind62 >> 24))),(uint8_t * ) (const byte[]) {255, 0})))))))));
                                    bind63 = 0;
                                    while (1)
                                        {
                                        if (((((uint8_t) list8Elem(bind64,1)) == 126) || (((uint8_t) list8Elem(bind64,1)) == 125)))
                                            {
                                            serialWrite(0,125);
                                            serialWrite(0,(list8Elem(bind64,1) ^ 32));
                                            }
                                        else
                                            {
                                            serialWrite(0,list8Elem(bind64,1));
                                            }
                                        if ((list8Len(bind64) == 2))
                                            {
                                            if (((((uint8_t) (list8Elem(bind64,0) + list8Elem(bind64,1))) == 126) || (((uint8_t) (list8Elem(bind64,0) + list8Elem(bind64,1))) == 125)))
                                                {
                                                serialWrite(0,125);
                                                serialWrite(0,((list8Elem(bind64,0) + list8Elem(bind64,1)) ^ 32));
                                                }
                                            else
                                                {
                                                serialWrite(0,(list8Elem(bind64,0) + list8Elem(bind64,1)));
                                                }
                                            break;
                                            }
                                        else
                                            {
                                            bind63 = 0;
                                            listAssign(&bind64, list8Cons((list8Elem(bind64,0) + list8Elem(bind64,1)),list8Slice(bind64,2,0)));
                                            }
                                        }
                                    serialWrite(0,126);
                                    listRelease(&bind64);
                                    }
                                else
                                    {
                                    if ((((uint8_t) list8Elem(bind5,0)) == 0))
                                        {
                                        int32_t bind69;
                                        uint8_t * bind70 = NULL;

                                        listAssign(&bind70, list8Cons(0,list8Cons(44,list8Slice(bind5,1,0))));
                                        bind69 = 0;
                                        while (1)
                                            {
                                            if (((((uint8_t) list8Elem(bind70,1)) == 126) || (((uint8_t) list8Elem(bind70,1)) == 125)))
                                                {
                                                serialWrite(0,125);
                                                serialWrite(0,(list8Elem(bind70,1) ^ 32));
                                                }
                                            else
                                                {
                                                serialWrite(0,list8Elem(bind70,1));
                                                }
                                            if ((list8Len(bind70) == 2))
                                                {
                                                if (((((uint8_t) (list8Elem(bind70,0) + list8Elem(bind70,1))) == 126) || (((uint8_t) (list8Elem(bind70,0) + list8Elem(bind70,1))) == 125)))
                                                    {
                                                    serialWrite(0,125);
                                                    serialWrite(0,((list8Elem(bind70,0) + list8Elem(bind70,1)) ^ 32));
                                                    }
                                                else
                                                    {
                                                    serialWrite(0,(list8Elem(bind70,0) + list8Elem(bind70,1)));
                                                    }
                                                break;
                                                }
                                            else
                                                {
                                                bind69 = 0;
                                                listAssign(&bind70, list8Cons((list8Elem(bind70,0) + list8Elem(bind70,1)),list8Slice(bind70,2,0)));
                                                }
                                            }
                                        serialWrite(0,126);
                                        listRelease(&bind70);
                                        }
                                    else
                                        {
                                        }
                                    }
                                }
                            }
                        }
                    }
                else
                    {
                    if ((((uint8_t) (list8Elem(bind5,0) & 240)) == 48))
                        {
                        if ((((uint8_t) list8Elem(bind5,0)) == 48))
                            {
                            if (((((uint8_t) list8Elem(bind5,(1 + 1))) == 2) && (((uint8_t) list8Elem(bind5,(1 + 2))) == 0)))
                                {
                                bool bind77;
                                int32_t bind78;
                                uint8_t * bind79 = NULL;

                                bind77 = digitalRead(list8Elem(bind5,(1 + 3)));
                                listAssign(&bind79, list8Cons(0,list8Cons(56,list8Cons(1,list8Cons(0,list8Cons(bind77 ? 1 : 0,(uint8_t * ) (const byte[]) {255, 0}))))));
                                bind78 = 0;
                                while (1)
                                    {
                                    if (((((uint8_t) list8Elem(bind79,1)) == 126) || (((uint8_t) list8Elem(bind79,1)) == 125)))
                                        {
                                        serialWrite(0,125);
                                        serialWrite(0,(list8Elem(bind79,1) ^ 32));
                                        }
                                    else
                                        {
                                        serialWrite(0,list8Elem(bind79,1));
                                        }
                                    if ((list8Len(bind79) == 2))
                                        {
                                        if (((((uint8_t) (list8Elem(bind79,0) + list8Elem(bind79,1))) == 126) || (((uint8_t) (list8Elem(bind79,0) + list8Elem(bind79,1))) == 125)))
                                            {
                                            serialWrite(0,125);
                                            serialWrite(0,((list8Elem(bind79,0) + list8Elem(bind79,1)) ^ 32));
                                            }
                                        else
                                            {
                                            serialWrite(0,(list8Elem(bind79,0) + list8Elem(bind79,1)));
                                            }
                                        break;
                                        }
                                    else
                                        {
                                        bind78 = 0;
                                        listAssign(&bind79, list8Cons((list8Elem(bind79,0) + list8Elem(bind79,1)),list8Slice(bind79,2,0)));
                                        }
                                    }
                                serialWrite(0,126);
                                listRelease(&bind79);
                                }
                            else
                                {
                                }
                            }
                        else
                            {
                            if ((((uint8_t) list8Elem(bind5,0)) == 49))
                                {
                                if (((((uint8_t) list8Elem(bind5,(1 + 0))) == 2) && ((((uint8_t) list8Elem(bind5,(1 + 1))) == 0) && ((((uint8_t) list8Elem(bind5,(1 + 3))) == 1) && (((uint8_t) list8Elem(bind5,(1 + 4))) == 0)))))
                                    {
                                    digitalWrite(list8Elem(bind5,(1 + 2)),(((uint8_t) list8Elem(bind5,(1 + 5))) == 0) ? 0 : 1);
                                    }
                                else
                                    {
                                    }
                                }
                            else
                                {
                                if ((((uint8_t) list8Elem(bind5,0)) == 50))
                                    {
                                    if (((((uint8_t) list8Elem(bind5,(1 + 1))) == 2) && ((((uint8_t) list8Elem(bind5,(1 + 2))) == 0) && ((((uint8_t) list8Elem(bind5,(1 + 4))) == 2) && (((uint8_t) list8Elem(bind5,(1 + 5))) == 0)))))
                                        {
                                        uint8_t bind87;
                                        int32_t bind88;
                                        uint8_t * bind89 = NULL;

                                        bind87 = digitalPortRead(list8Elem(bind5,(1 + 3)),list8Elem(bind5,(1 + 6)));
                                        listAssign(&bind89, list8Cons(0,list8Cons(57,list8Cons(2,list8Cons(0,list8Cons(bind87,(uint8_t * ) (const byte[]) {255, 0}))))));
                                        bind88 = 0;
                                        while (1)
                                            {
                                            if (((((uint8_t) list8Elem(bind89,1)) == 126) || (((uint8_t) list8Elem(bind89,1)) == 125)))
                                                {
                                                serialWrite(0,125);
                                                serialWrite(0,(list8Elem(bind89,1) ^ 32));
                                                }
                                            else
                                                {
                                                serialWrite(0,list8Elem(bind89,1));
                                                }
                                            if ((list8Len(bind89) == 2))
                                                {
                                                if (((((uint8_t) (list8Elem(bind89,0) + list8Elem(bind89,1))) == 126) || (((uint8_t) (list8Elem(bind89,0) + list8Elem(bind89,1))) == 125)))
                                                    {
                                                    serialWrite(0,125);
                                                    serialWrite(0,((list8Elem(bind89,0) + list8Elem(bind89,1)) ^ 32));
                                                    }
                                                else
                                                    {
                                                    serialWrite(0,(list8Elem(bind89,0) + list8Elem(bind89,1)));
                                                    }
                                                break;
                                                }
                                            else
                                                {
                                                bind88 = 0;
                                                listAssign(&bind89, list8Cons((list8Elem(bind89,0) + list8Elem(bind89,1)),list8Slice(bind89,2,0)));
                                                }
                                            }
                                        serialWrite(0,126);
                                        listRelease(&bind89);
                                        }
                                    else
                                        {
                                        }
                                    }
                                else
                                    {
                                    if ((((uint8_t) list8Elem(bind5,0)) == 51))
                                        {
                                        if (((((uint8_t) list8Elem(bind5,(1 + 0))) == 2) && ((((uint8_t) list8Elem(bind5,(1 + 1))) == 0) && ((((uint8_t) list8Elem(bind5,(1 + 3))) == 2) && ((((uint8_t) list8Elem(bind5,(1 + 4))) == 0) && ((((uint8_t) list8Elem(bind5,(1 + 6))) == 2) && (((uint8_t) list8Elem(bind5,(1 + 7))) == 0)))))))
                                            {
                                            digitalPortWrite(list8Elem(bind5,(1 + 2)),list8Elem(bind5,(1 + 5)),list8Elem(bind5,(1 + 8)));
                                            }
                                        else
                                            {
                                            }
                                        }
                                    else
                                        {
                                        }
                                    }
                                }
                            }
                        }
                    else
                        {
                        if ((((uint8_t) (list8Elem(bind5,0) & 240)) == 64))
                            {
                            if ((((uint8_t) list8Elem(bind5,0)) == 64))
                                {
                                if (((((uint8_t) list8Elem(bind5,(1 + 1))) == 2) && (((uint8_t) list8Elem(bind5,(1 + 2))) == 0)))
                                    {
                                    uint16_t bind98;
                                    int32_t bind99;
                                    uint8_t * bind100 = NULL;

                                    bind98 = analogRead(list8Elem(bind5,(1 + 3)));
                                    listAssign(&bind100, list8Cons(0,list8Cons(72,list8Cons(3,list8Cons(0,list8Cons(((uint8_t) ((int32_t) (bind98 & 255))),list8Cons(((uint8_t) ((int32_t) (bind98 >> 8))),(uint8_t * ) (const byte[]) {255, 0})))))));
                                    bind99 = 0;
                                    while (1)
                                        {
                                        if (((((uint8_t) list8Elem(bind100,1)) == 126) || (((uint8_t) list8Elem(bind100,1)) == 125)))
                                            {
                                            serialWrite(0,125);
                                            serialWrite(0,(list8Elem(bind100,1) ^ 32));
                                            }
                                        else
                                            {
                                            serialWrite(0,list8Elem(bind100,1));
                                            }
                                        if ((list8Len(bind100) == 2))
                                            {
                                            if (((((uint8_t) (list8Elem(bind100,0) + list8Elem(bind100,1))) == 126) || (((uint8_t) (list8Elem(bind100,0) + list8Elem(bind100,1))) == 125)))
                                                {
                                                serialWrite(0,125);
                                                serialWrite(0,((list8Elem(bind100,0) + list8Elem(bind100,1)) ^ 32));
                                                }
                                            else
                                                {
                                                serialWrite(0,(list8Elem(bind100,0) + list8Elem(bind100,1)));
                                                }
                                            break;
                                            }
                                        else
                                            {
                                            bind99 = 0;
                                            listAssign(&bind100, list8Cons((list8Elem(bind100,0) + list8Elem(bind100,1)),list8Slice(bind100,2,0)));
                                            }
                                        }
                                    serialWrite(0,126);
                                    listRelease(&bind100);
                                    }
                                else
                                    {
                                    }
                                }
                            else
                                {
                                if ((((uint8_t) list8Elem(bind5,0)) == 65))
                                    {
                                    if (((((uint8_t) list8Elem(bind5,(1 + 0))) == 2) && ((((uint8_t) list8Elem(bind5,(1 + 1))) == 0) && ((((uint8_t) list8Elem(bind5,(1 + 3))) == 3) && (((uint8_t) list8Elem(bind5,(1 + 4))) == 0)))))
                                        {
                                        analogWrite(list8Elem(bind5,(1 + 2)),((((uint16_t) ((int32_t) list8Elem(bind5,(1 + 6)))) << 8) | ((uint16_t) ((int32_t) list8Elem(bind5,(1 + 5))))));
                                        }
                                    else
                                        {
                                        }
                                    }
                                else
                                    {
                                    if ((((uint8_t) list8Elem(bind5,0)) == 66))
                                        {
                                        if (((((uint8_t) list8Elem(bind5,(1 + 0))) == 2) && ((((uint8_t) list8Elem(bind5,(1 + 1))) == 0) && ((((uint8_t) list8Elem(bind5,(1 + 3))) == 3) && ((((uint8_t) list8Elem(bind5,(1 + 4))) == 0) && ((((uint8_t) list8Elem(bind5,(1 + 7))) == 4) && (((uint8_t) list8Elem(bind5,(1 + 8))) == 0)))))))
                                            {
                                            if ((((((((uint32_t) ((int32_t) list8Elem(bind5,(1 + 11)))) << 24) | (((uint32_t) ((int32_t) list8Elem(bind5,(1 + 10)))) << 16)) | (((uint32_t) ((int32_t) list8Elem(bind5,(1 + 9)))) << 8)) | ((uint32_t) ((int32_t) list8Elem(bind5,(1 + 8))))) == 0))
                                                {
                                                tone(list8Elem(bind5,(1 + 2)),((((uint16_t) ((int32_t) list8Elem(bind5,(1 + 6)))) << 8) | ((uint16_t) ((int32_t) list8Elem(bind5,(1 + 5))))),0);
                                                }
                                            else
                                                {
                                                tone(list8Elem(bind5,(1 + 2)),((((uint16_t) ((int32_t) list8Elem(bind5,(1 + 6)))) << 8) | ((uint16_t) ((int32_t) list8Elem(bind5,(1 + 5))))),((((((uint32_t) ((int32_t) list8Elem(bind5,(1 + 11)))) << 24) | (((uint32_t) ((int32_t) list8Elem(bind5,(1 + 10)))) << 16)) | (((uint32_t) ((int32_t) list8Elem(bind5,(1 + 9)))) << 8)) | ((uint32_t) ((int32_t) list8Elem(bind5,(1 + 8))))));
                                                }
                                            }
                                        else
                                            {
                                            }
                                        }
                                    else
                                        {
                                        if ((((uint8_t) list8Elem(bind5,0)) == 67))
                                            {
                                            if (((((uint8_t) list8Elem(bind5,(1 + 0))) == 2) && (((uint8_t) list8Elem(bind5,(1 + 1))) == 0)))
                                                {
                                                noTone(list8Elem(bind5,(1 + 2)));
                                                }
                                            else
                                                {
                                                }
                                            }
                                        else
                                            {
                                            }
                                        }
                                    }
                                }
                            }
                        else
                            {
                            if ((((uint8_t) (list8Elem(bind5,0) & 240)) == 80))
                                {
                                if ((((uint8_t) list8Elem(bind5,0)) == 80))
                                    {
                                    i2cConfig();
                                    }
                                else
                                    {
                                    if ((((uint8_t) list8Elem(bind5,0)) == 81))
                                        {
                                        if (((((uint8_t) list8Elem(bind5,(1 + 1))) == 2) && ((((uint8_t) list8Elem(bind5,(1 + 2))) == 0) && ((((uint8_t) list8Elem(bind5,(1 + 4))) == 2) && (((uint8_t) list8Elem(bind5,(1 + 5))) == 0)))))
                                            {
                                            uint8_t * bind115 = NULL;
                                            int32_t bind116;
                                            uint8_t * bind117 = NULL;

                                            listAssign(&bind115, i2cRead(list8Elem(bind5,(1 + 3)),list8Elem(bind5,(1 + 6))));
                                            listAssign(&bind117, list8Cons(0,list8Cons(88,list8Cons(8,list8Cons(0,list8Cons(((uint8_t) list8Len(bind115)),bind115))))));
                                            bind116 = 0;
                                            while (1)
                                                {
                                                if (((((uint8_t) list8Elem(bind117,1)) == 126) || (((uint8_t) list8Elem(bind117,1)) == 125)))
                                                    {
                                                    serialWrite(0,125);
                                                    serialWrite(0,(list8Elem(bind117,1) ^ 32));
                                                    }
                                                else
                                                    {
                                                    serialWrite(0,list8Elem(bind117,1));
                                                    }
                                                if ((list8Len(bind117) == 2))
                                                    {
                                                    if (((((uint8_t) (list8Elem(bind117,0) + list8Elem(bind117,1))) == 126) || (((uint8_t) (list8Elem(bind117,0) + list8Elem(bind117,1))) == 125)))
                                                        {
                                                        serialWrite(0,125);
                                                        serialWrite(0,((list8Elem(bind117,0) + list8Elem(bind117,1)) ^ 32));
                                                        }
                                                    else
                                                        {
                                                        serialWrite(0,(list8Elem(bind117,0) + list8Elem(bind117,1)));
                                                        }
                                                    break;
                                                    }
                                                else
                                                    {
                                                    bind116 = 0;
                                                    listAssign(&bind117, list8Cons((list8Elem(bind117,0) + list8Elem(bind117,1)),list8Slice(bind117,2,0)));
                                                    }
                                                }
                                            serialWrite(0,126);
                                            listRelease(&bind115);
                                            listRelease(&bind117);
                                            }
                                        else
                                            {
                                            }
                                        }
                                    else
                                        {
                                        if ((((uint8_t) list8Elem(bind5,0)) == 82))
                                            {
                                            if (((((uint8_t) list8Elem(bind5,(1 + 0))) == 2) && ((((uint8_t) list8Elem(bind5,(1 + 1))) == 0) && ((((uint8_t) list8Elem(bind5,(1 + 3))) == 8) && ((((uint8_t) list8Elem(bind5,(1 + 4))) == 0) && ((list8Len(bind5) - 1) == (((int32_t) list8Elem(bind5,(1 + 5))) + 6)))))))
                                                {
                                                i2cWrite(list8Elem(bind5,(1 + 2)),list8Slice(list8Slice(bind5,1,0),6,0));
                                                }
                                            else
                                                {
                                                }
                                            }
                                        else
                                            {
                                            }
                                        }
                                    }
                                }
                            else
                                {
                                if ((((uint8_t) (list8Elem(bind5,0) & 240)) == 224))
                                    {
                                    if ((((uint8_t) list8Elem(bind5,0)) == 224))
                                        {
                                        if (((((uint8_t) list8Elem(bind5,(1 + 0))) == 2) && ((((uint8_t) list8Elem(bind5,(1 + 1))) == 0) && ((((uint8_t) list8Elem(bind5,(1 + 3))) == 4) && (((uint8_t) list8Elem(bind5,(1 + 4))) == 0)))))
                                            {
                                            serialBegin(list8Elem(bind5,(1 + 2)),((((((uint32_t) ((int32_t) list8Elem(bind5,(1 + 8)))) << 24) | (((uint32_t) ((int32_t) list8Elem(bind5,(1 + 7)))) << 16)) | (((uint32_t) ((int32_t) list8Elem(bind5,(1 + 6)))) << 8)) | ((uint32_t) ((int32_t) list8Elem(bind5,(1 + 5))))));
                                            }
                                        else
                                            {
                                            }
                                        }
                                    else
                                        {
                                        if ((((uint8_t) list8Elem(bind5,0)) == 225))
                                            {
                                            if (((((uint8_t) list8Elem(bind5,(1 + 0))) == 2) && (((uint8_t) list8Elem(bind5,(1 + 1))) == 0)))
                                                {
                                                serialEnd(list8Elem(bind5,(1 + 2)));
                                                }
                                            else
                                                {
                                                }
                                            }
                                        else
                                            {
                                            if ((((uint8_t) list8Elem(bind5,0)) == 227))
                                                {
                                                if (((((uint8_t) list8Elem(bind5,(1 + 1))) == 2) && (((uint8_t) list8Elem(bind5,(1 + 2))) == 0)))
                                                    {
                                                    int32_t bind130;
                                                    int32_t bind131;
                                                    uint8_t * bind132 = NULL;

                                                    bind130 = serialRead(list8Elem(bind5,(1 + 3)));
                                                    listAssign(&bind132, list8Cons(0,list8Cons(233,list8Cons(4,list8Cons(0,list8Cons(((uint8_t) ((int32_t) (bind130 & 255))),list8Cons(((uint8_t) ((int32_t) (bind130 >> 8))),list8Cons(((uint8_t) ((int32_t) (bind130 >> 16))),list8Cons(((uint8_t) ((int32_t) (bind130 >> 24))),(uint8_t * ) (const byte[]) {255, 0})))))))));
                                                    bind131 = 0;
                                                    while (1)
                                                        {
                                                        if (((((uint8_t) list8Elem(bind132,1)) == 126) || (((uint8_t) list8Elem(bind132,1)) == 125)))
                                                            {
                                                            serialWrite(0,125);
                                                            serialWrite(0,(list8Elem(bind132,1) ^ 32));
                                                            }
                                                        else
                                                            {
                                                            serialWrite(0,list8Elem(bind132,1));
                                                            }
                                                        if ((list8Len(bind132) == 2))
                                                            {
                                                            if (((((uint8_t) (list8Elem(bind132,0) + list8Elem(bind132,1))) == 126) || (((uint8_t) (list8Elem(bind132,0) + list8Elem(bind132,1))) == 125)))
                                                                {
                                                                serialWrite(0,125);
                                                                serialWrite(0,((list8Elem(bind132,0) + list8Elem(bind132,1)) ^ 32));
                                                                }
                                                            else
                                                                {
                                                                serialWrite(0,(list8Elem(bind132,0) + list8Elem(bind132,1)));
                                                                }
                                                            break;
                                                            }
                                                        else
                                                            {
                                                            bind131 = 0;
                                                            listAssign(&bind132, list8Cons((list8Elem(bind132,0) + list8Elem(bind132,1)),list8Slice(bind132,2,0)));
                                                            }
                                                        }
                                                    serialWrite(0,126);
                                                    listRelease(&bind132);
                                                    }
                                                else
                                                    {
                                                    }
                                                }
                                            else
                                                {
                                                if ((((uint8_t) list8Elem(bind5,0)) == 228))
                                                    {
                                                    if (((((uint8_t) list8Elem(bind5,(1 + 1))) == 2) && (((uint8_t) list8Elem(bind5,(1 + 2))) == 0)))
                                                        {
                                                        uint8_t * bind138 = NULL;
                                                        int32_t bind139;
                                                        uint8_t * bind140 = NULL;

                                                        listAssign(&bind138, serialReadList(list8Elem(bind5,(1 + 3))));
                                                        listAssign(&bind140, list8Cons(0,list8Cons(234,list8Cons(8,list8Cons(0,list8Cons(((uint8_t) list8Len(bind138)),bind138))))));
                                                        bind139 = 0;
                                                        while (1)
                                                            {
                                                            if (((((uint8_t) list8Elem(bind140,1)) == 126) || (((uint8_t) list8Elem(bind140,1)) == 125)))
                                                                {
                                                                serialWrite(0,125);
                                                                serialWrite(0,(list8Elem(bind140,1) ^ 32));
                                                                }
                                                            else
                                                                {
                                                                serialWrite(0,list8Elem(bind140,1));
                                                                }
                                                            if ((list8Len(bind140) == 2))
                                                                {
                                                                if (((((uint8_t) (list8Elem(bind140,0) + list8Elem(bind140,1))) == 126) || (((uint8_t) (list8Elem(bind140,0) + list8Elem(bind140,1))) == 125)))
                                                                    {
                                                                    serialWrite(0,125);
                                                                    serialWrite(0,((list8Elem(bind140,0) + list8Elem(bind140,1)) ^ 32));
                                                                    }
                                                                else
                                                                    {
                                                                    serialWrite(0,(list8Elem(bind140,0) + list8Elem(bind140,1)));
                                                                    }
                                                                break;
                                                                }
                                                            else
                                                                {
                                                                bind139 = 0;
                                                                listAssign(&bind140, list8Cons((list8Elem(bind140,0) + list8Elem(bind140,1)),list8Slice(bind140,2,0)));
                                                                }
                                                            }
                                                        serialWrite(0,126);
                                                        listRelease(&bind138);
                                                        listRelease(&bind140);
                                                        }
                                                    else
                                                        {
                                                        }
                                                    }
                                                else
                                                    {
                                                    if ((((uint8_t) list8Elem(bind5,0)) == 229))
                                                        {
                                                        if (((((uint8_t) list8Elem(bind5,(1 + 0))) == 2) && ((((uint8_t) list8Elem(bind5,(1 + 1))) == 0) && ((((uint8_t) list8Elem(bind5,(1 + 3))) == 2) && (((uint8_t) list8Elem(bind5,(1 + 4))) == 0)))))
                                                            {
                                                            serialWrite(list8Elem(bind5,(1 + 2)),list8Elem(bind5,(1 + 5)));
                                                            }
                                                        else
                                                            {
                                                            }
                                                        }
                                                    else
                                                        {
                                                        if ((((uint8_t) list8Elem(bind5,0)) == 230))
                                                            {
                                                            if (((((uint8_t) list8Elem(bind5,(1 + 0))) == 2) && ((((uint8_t) list8Elem(bind5,(1 + 1))) == 0) && ((((uint8_t) list8Elem(bind5,(1 + 3))) == 8) && ((((uint8_t) list8Elem(bind5,(1 + 4))) == 0) && ((list8Len(bind5) - 1) == (((int32_t) list8Elem(bind5,(1 + 5))) + 6)))))))
                                                                {
                                                                serialWriteList(list8Elem(bind5,(1 + 2)),list8Slice(list8Slice(bind5,1,0),6,0));
                                                                }
                                                            else
                                                                {
                                                                }
                                                            }
                                                        else
                                                            {
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                else
                                    {
                                    if ((((uint8_t) (list8Elem(bind5,0) & 240)) == 96))
                                        {
                                        if ((((uint8_t) list8Elem(bind5,0)) == 96))
                                            {
                                            if (((((uint8_t) list8Elem(bind5,(1 + 1))) == 3) && ((((uint8_t) list8Elem(bind5,(1 + 2))) == 0) && ((((uint8_t) list8Elem(bind5,(1 + 5))) == 2) && ((((uint8_t) list8Elem(bind5,(1 + 6))) == 0) && ((((uint8_t) list8Elem(bind5,(1 + 8))) == 2) && (((uint8_t) list8Elem(bind5,(1 + 9))) == 0)))))))
                                                {
                                                uint8_t bind151;
                                                int32_t bind152;
                                                uint8_t * bind153 = NULL;

                                                bind151 = stepper2Pin(((((uint16_t) ((int32_t) list8Elem(bind5,(1 + 4)))) << 8) | ((uint16_t) ((int32_t) list8Elem(bind5,(1 + 3))))),list8Elem(bind5,(1 + 7)),list8Elem(bind5,(1 + 10)));
                                                listAssign(&bind153, list8Cons(0,list8Cons(104,list8Cons(2,list8Cons(0,list8Cons(bind151,(uint8_t * ) (const byte[]) {255, 0}))))));
                                                bind152 = 0;
                                                while (1)
                                                    {
                                                    if (((((uint8_t) list8Elem(bind153,1)) == 126) || (((uint8_t) list8Elem(bind153,1)) == 125)))
                                                        {
                                                        serialWrite(0,125);
                                                        serialWrite(0,(list8Elem(bind153,1) ^ 32));
                                                        }
                                                    else
                                                        {
                                                        serialWrite(0,list8Elem(bind153,1));
                                                        }
                                                    if ((list8Len(bind153) == 2))
                                                        {
                                                        if (((((uint8_t) (list8Elem(bind153,0) + list8Elem(bind153,1))) == 126) || (((uint8_t) (list8Elem(bind153,0) + list8Elem(bind153,1))) == 125)))
                                                            {
                                                            serialWrite(0,125);
                                                            serialWrite(0,((list8Elem(bind153,0) + list8Elem(bind153,1)) ^ 32));
                                                            }
                                                        else
                                                            {
                                                            serialWrite(0,(list8Elem(bind153,0) + list8Elem(bind153,1)));
                                                            }
                                                        break;
                                                        }
                                                    else
                                                        {
                                                        bind152 = 0;
                                                        listAssign(&bind153, list8Cons((list8Elem(bind153,0) + list8Elem(bind153,1)),list8Slice(bind153,2,0)));
                                                        }
                                                    }
                                                serialWrite(0,126);
                                                listRelease(&bind153);
                                                }
                                            else
                                                {
                                                }
                                            }
                                        else
                                            {
                                            if ((((uint8_t) list8Elem(bind5,0)) == 97))
                                                {
                                                if (((((uint8_t) list8Elem(bind5,(1 + 1))) == 3) && ((((uint8_t) list8Elem(bind5,(1 + 2))) == 0) && ((((uint8_t) list8Elem(bind5,(1 + 5))) == 2) && ((((uint8_t) list8Elem(bind5,(1 + 6))) == 0) && ((((uint8_t) list8Elem(bind5,(1 + 8))) == 2) && ((((uint8_t) list8Elem(bind5,(1 + 9))) == 0) && ((((uint8_t) list8Elem(bind5,(1 + 11))) == 2) && ((((uint8_t) list8Elem(bind5,(1 + 12))) == 0) && ((((uint8_t) list8Elem(bind5,(1 + 14))) == 2) && (((uint8_t) list8Elem(bind5,(1 + 15))) == 0)))))))))))
                                                    {
                                                    uint8_t bind159;
                                                    int32_t bind160;
                                                    uint8_t * bind161 = NULL;

                                                    bind159 = stepper4Pin(((((((uint16_t) ((int32_t) list8Elem(bind5,(1 + 6)))) << 24) | (((uint16_t) ((int32_t) list8Elem(bind5,(1 + 5)))) << 16)) | (((uint16_t) ((int32_t) list8Elem(bind5,(1 + 4)))) << 8)) | ((uint16_t) ((int32_t) list8Elem(bind5,(1 + 3))))),list8Elem(bind5,(1 + 7)),list8Elem(bind5,(1 + 10)),list8Elem(bind5,(1 + 13)),list8Elem(bind5,(1 + 16)));
                                                    listAssign(&bind161, list8Cons(0,list8Cons(104,list8Cons(2,list8Cons(0,list8Cons(bind159,(uint8_t * ) (const byte[]) {255, 0}))))));
                                                    bind160 = 0;
                                                    while (1)
                                                        {
                                                        if (((((uint8_t) list8Elem(bind161,1)) == 126) || (((uint8_t) list8Elem(bind161,1)) == 125)))
                                                            {
                                                            serialWrite(0,125);
                                                            serialWrite(0,(list8Elem(bind161,1) ^ 32));
                                                            }
                                                        else
                                                            {
                                                            serialWrite(0,list8Elem(bind161,1));
                                                            }
                                                        if ((list8Len(bind161) == 2))
                                                            {
                                                            if (((((uint8_t) (list8Elem(bind161,0) + list8Elem(bind161,1))) == 126) || (((uint8_t) (list8Elem(bind161,0) + list8Elem(bind161,1))) == 125)))
                                                                {
                                                                serialWrite(0,125);
                                                                serialWrite(0,((list8Elem(bind161,0) + list8Elem(bind161,1)) ^ 32));
                                                                }
                                                            else
                                                                {
                                                                serialWrite(0,(list8Elem(bind161,0) + list8Elem(bind161,1)));
                                                                }
                                                            break;
                                                            }
                                                        else
                                                            {
                                                            bind160 = 0;
                                                            listAssign(&bind161, list8Cons((list8Elem(bind161,0) + list8Elem(bind161,1)),list8Slice(bind161,2,0)));
                                                            }
                                                        }
                                                    serialWrite(0,126);
                                                    listRelease(&bind161);
                                                    }
                                                else
                                                    {
                                                    }
                                                }
                                            else
                                                {
                                                if ((((uint8_t) list8Elem(bind5,0)) == 98))
                                                    {
                                                    if (((((uint8_t) list8Elem(bind5,(1 + 0))) == 2) && ((((uint8_t) list8Elem(bind5,(1 + 1))) == 0) && ((((uint8_t) list8Elem(bind5,(1 + 3))) == 4) && (((uint8_t) list8Elem(bind5,(1 + 4))) == 0)))))
                                                        {
                                                        stepperSetSpeed(list8Elem(bind5,(1 + 2)),((((((int32_t) ((int32_t) list8Elem(bind5,(1 + 8)))) << 24) | (((int32_t) ((int32_t) list8Elem(bind5,(1 + 7)))) << 16)) | (((int32_t) ((int32_t) list8Elem(bind5,(1 + 6)))) << 8)) | ((int32_t) ((int32_t) list8Elem(bind5,(1 + 5))))));
                                                        }
                                                    else
                                                        {
                                                        }
                                                    }
                                                else
                                                    {
                                                    if ((((uint8_t) list8Elem(bind5,0)) == 99))
                                                        {
                                                        if (((((uint8_t) list8Elem(bind5,(1 + 1))) == 2) && ((((uint8_t) list8Elem(bind5,(1 + 2))) == 0) && ((((uint8_t) list8Elem(bind5,(1 + 4))) == 3) && (((uint8_t) list8Elem(bind5,(1 + 5))) == 0)))))
                                                            {
                                                            int32_t bind169;
                                                            uint8_t * bind170 = NULL;

                                                            stepperSetSpeed(list8Elem(bind5,(1 + 3)),((((int32_t) ((int32_t) list8Elem(bind5,(1 + 7)))) << 8) | ((int32_t) ((int32_t) list8Elem(bind5,(1 + 6))))));
                                                            listAssign(&bind170, list8Cons(0,list8Cons(106,(uint8_t * ) (const byte[]) {255, 0})));
                                                            bind169 = 0;
                                                            while (1)
                                                                {
                                                                if (((((uint8_t) list8Elem(bind170,1)) == 126) || (((uint8_t) list8Elem(bind170,1)) == 125)))
                                                                    {
                                                                    serialWrite(0,125);
                                                                    serialWrite(0,(list8Elem(bind170,1) ^ 32));
                                                                    }
                                                                else
                                                                    {
                                                                    serialWrite(0,list8Elem(bind170,1));
                                                                    }
                                                                if ((list8Len(bind170) == 2))
                                                                    {
                                                                    if (((((uint8_t) (list8Elem(bind170,0) + list8Elem(bind170,1))) == 126) || (((uint8_t) (list8Elem(bind170,0) + list8Elem(bind170,1))) == 125)))
                                                                        {
                                                                        serialWrite(0,125);
                                                                        serialWrite(0,((list8Elem(bind170,0) + list8Elem(bind170,1)) ^ 32));
                                                                        }
                                                                    else
                                                                        {
                                                                        serialWrite(0,(list8Elem(bind170,0) + list8Elem(bind170,1)));
                                                                        }
                                                                    break;
                                                                    }
                                                                else
                                                                    {
                                                                    bind169 = 0;
                                                                    listAssign(&bind170, list8Cons((list8Elem(bind170,0) + list8Elem(bind170,1)),list8Slice(bind170,2,0)));
                                                                    }
                                                                }
                                                            serialWrite(0,126);
                                                            listRelease(&bind170);
                                                            }
                                                        else
                                                            {
                                                            }
                                                        }
                                                    else
                                                        {
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    else
                                        {
                                        if ((((uint8_t) (list8Elem(bind5,0) & 240)) == 128))
                                            {
                                            if ((((uint8_t) list8Elem(bind5,0)) == 128))
                                                {
                                                if (((((uint8_t) list8Elem(bind5,(1 + 1))) == 2) && ((((uint8_t) list8Elem(bind5,(1 + 2))) == 0) && ((((uint8_t) list8Elem(bind5,(1 + 4))) == 3) && ((((uint8_t) list8Elem(bind5,(1 + 5))) == 0) && ((((uint8_t) list8Elem(bind5,(1 + 8))) == 3) && (((uint8_t) list8Elem(bind5,(1 + 9))) == 0)))))))
                                                    {
                                                    uint8_t bind177;
                                                    int32_t bind178;
                                                    uint8_t * bind179 = NULL;

                                                    bind177 = servoAttachMinMax(list8Elem(bind5,(1 + 3)),((((int16_t) ((int32_t) list8Elem(bind5,(1 + 7)))) << 8) | ((int16_t) ((int32_t) list8Elem(bind5,(1 + 6))))),((((int16_t) ((int32_t) list8Elem(bind5,(1 + 11)))) << 8) | ((int16_t) ((int32_t) list8Elem(bind5,(1 + 10))))));
                                                    listAssign(&bind179, list8Cons(0,list8Cons(136,list8Cons(2,list8Cons(0,list8Cons(bind177,(uint8_t * ) (const byte[]) {255, 0}))))));
                                                    bind178 = 0;
                                                    while (1)
                                                        {
                                                        if (((((uint8_t) list8Elem(bind179,1)) == 126) || (((uint8_t) list8Elem(bind179,1)) == 125)))
                                                            {
                                                            serialWrite(0,125);
                                                            serialWrite(0,(list8Elem(bind179,1) ^ 32));
                                                            }
                                                        else
                                                            {
                                                            serialWrite(0,list8Elem(bind179,1));
                                                            }
                                                        if ((list8Len(bind179) == 2))
                                                            {
                                                            if (((((uint8_t) (list8Elem(bind179,0) + list8Elem(bind179,1))) == 126) || (((uint8_t) (list8Elem(bind179,0) + list8Elem(bind179,1))) == 125)))
                                                                {
                                                                serialWrite(0,125);
                                                                serialWrite(0,((list8Elem(bind179,0) + list8Elem(bind179,1)) ^ 32));
                                                                }
                                                            else
                                                                {
                                                                serialWrite(0,(list8Elem(bind179,0) + list8Elem(bind179,1)));
                                                                }
                                                            break;
                                                            }
                                                        else
                                                            {
                                                            bind178 = 0;
                                                            listAssign(&bind179, list8Cons((list8Elem(bind179,0) + list8Elem(bind179,1)),list8Slice(bind179,2,0)));
                                                            }
                                                        }
                                                    serialWrite(0,126);
                                                    listRelease(&bind179);
                                                    }
                                                else
                                                    {
                                                    }
                                                }
                                            else
                                                {
                                                if ((((uint8_t) list8Elem(bind5,0)) == 129))
                                                    {
                                                    if (((((uint8_t) list8Elem(bind5,(1 + 0))) == 2) && (((uint8_t) list8Elem(bind5,(1 + 1))) == 0)))
                                                        {
                                                        servoDetach(list8Elem(bind5,(1 + 2)));
                                                        }
                                                    else
                                                        {
                                                        }
                                                    }
                                                else
                                                    {
                                                    if ((((uint8_t) list8Elem(bind5,0)) == 130))
                                                        {
                                                        if (((((uint8_t) list8Elem(bind5,(1 + 0))) == 2) && ((((uint8_t) list8Elem(bind5,(1 + 1))) == 0) && ((((uint8_t) list8Elem(bind5,(1 + 3))) == 3) && (((uint8_t) list8Elem(bind5,(1 + 4))) == 0)))))
                                                            {
                                                            servoWrite(list8Elem(bind5,(1 + 2)),((((int16_t) ((int32_t) list8Elem(bind5,(1 + 5)))) << 8) | ((int16_t) ((int32_t) list8Elem(bind5,(1 + 6))))));
                                                            }
                                                        else
                                                            {
                                                            }
                                                        }
                                                    else
                                                        {
                                                        if ((((uint8_t) list8Elem(bind5,0)) == 131))
                                                            {
                                                            if (((((uint8_t) list8Elem(bind5,(1 + 0))) == 2) && ((((uint8_t) list8Elem(bind5,(1 + 1))) == 0) && ((((uint8_t) list8Elem(bind5,(1 + 3))) == 3) && (((uint8_t) list8Elem(bind5,(1 + 4))) == 0)))))
                                                                {
                                                                servoWrite(list8Elem(bind5,(1 + 2)),((((int16_t) ((int32_t) list8Elem(bind5,(1 + 6)))) << 8) | ((int16_t) ((int32_t) list8Elem(bind5,(1 + 5))))));
                                                                }
                                                            else
                                                                {
                                                                }
                                                            }
                                                        else
                                                            {
                                                            if ((((uint8_t) list8Elem(bind5,0)) == 132))
                                                                {
                                                                if (((((uint8_t) list8Elem(bind5,(1 + 1))) == 2) && (((uint8_t) list8Elem(bind5,(1 + 2))) == 0)))
                                                                    {
                                                                    uint16_t bind191;
                                                                    int32_t bind192;
                                                                    uint8_t * bind193 = NULL;

                                                                    bind191 = servoRead(list8Elem(bind5,(1 + 3)));
                                                                    listAssign(&bind193, list8Cons(0,list8Cons(137,list8Cons(3,list8Cons(0,list8Cons(((uint8_t) ((int32_t) (bind191 & 255))),list8Cons(((uint8_t) ((int32_t) (bind191 >> 8))),(uint8_t * ) (const byte[]) {255, 0})))))));
                                                                    bind192 = 0;
                                                                    while (1)
                                                                        {
                                                                        if (((((uint8_t) list8Elem(bind193,1)) == 126) || (((uint8_t) list8Elem(bind193,1)) == 125)))
                                                                            {
                                                                            serialWrite(0,125);
                                                                            serialWrite(0,(list8Elem(bind193,1) ^ 32));
                                                                            }
                                                                        else
                                                                            {
                                                                            serialWrite(0,list8Elem(bind193,1));
                                                                            }
                                                                        if ((list8Len(bind193) == 2))
                                                                            {
                                                                            if (((((uint8_t) (list8Elem(bind193,0) + list8Elem(bind193,1))) == 126) || (((uint8_t) (list8Elem(bind193,0) + list8Elem(bind193,1))) == 125)))
                                                                                {
                                                                                serialWrite(0,125);
                                                                                serialWrite(0,((list8Elem(bind193,0) + list8Elem(bind193,1)) ^ 32));
                                                                                }
                                                                            else
                                                                                {
                                                                                serialWrite(0,(list8Elem(bind193,0) + list8Elem(bind193,1)));
                                                                                }
                                                                            break;
                                                                            }
                                                                        else
                                                                            {
                                                                            bind192 = 0;
                                                                            listAssign(&bind193, list8Cons((list8Elem(bind193,0) + list8Elem(bind193,1)),list8Slice(bind193,2,0)));
                                                                            }
                                                                        }
                                                                    serialWrite(0,126);
                                                                    listRelease(&bind193);
                                                                    }
                                                                else
                                                                    {
                                                                    }
                                                                }
                                                            else
                                                                {
                                                                if ((((uint8_t) list8Elem(bind5,0)) == 133))
                                                                    {
                                                                    if (((((uint8_t) list8Elem(bind5,(1 + 1))) == 2) && (((uint8_t) list8Elem(bind5,(1 + 2))) == 0)))
                                                                        {
                                                                        uint16_t bind199;
                                                                        int32_t bind200;
                                                                        uint8_t * bind201 = NULL;

                                                                        bind199 = servoReadMicros(list8Elem(bind5,(1 + 3)));
                                                                        listAssign(&bind201, list8Cons(0,list8Cons(137,list8Cons(3,list8Cons(0,list8Cons(((uint8_t) ((int32_t) (bind199 & 255))),list8Cons(((uint8_t) ((int32_t) (bind199 >> 8))),(uint8_t * ) (const byte[]) {255, 0})))))));
                                                                        bind200 = 0;
                                                                        while (1)
                                                                            {
                                                                            if (((((uint8_t) list8Elem(bind201,1)) == 126) || (((uint8_t) list8Elem(bind201,1)) == 125)))
                                                                                {
                                                                                serialWrite(0,125);
                                                                                serialWrite(0,(list8Elem(bind201,1) ^ 32));
                                                                                }
                                                                            else
                                                                                {
                                                                                serialWrite(0,list8Elem(bind201,1));
                                                                                }
                                                                            if ((list8Len(bind201) == 2))
                                                                                {
                                                                                if (((((uint8_t) (list8Elem(bind201,0) + list8Elem(bind201,1))) == 126) || (((uint8_t) (list8Elem(bind201,0) + list8Elem(bind201,1))) == 125)))
                                                                                    {
                                                                                    serialWrite(0,125);
                                                                                    serialWrite(0,((list8Elem(bind201,0) + list8Elem(bind201,1)) ^ 32));
                                                                                    }
                                                                                else
                                                                                    {
                                                                                    serialWrite(0,(list8Elem(bind201,0) + list8Elem(bind201,1)));
                                                                                    }
                                                                                break;
                                                                                }
                                                                            else
                                                                                {
                                                                                bind200 = 0;
                                                                                listAssign(&bind201, list8Cons((list8Elem(bind201,0) + list8Elem(bind201,1)),list8Slice(bind201,2,0)));
                                                                                }
                                                                            }
                                                                        serialWrite(0,126);
                                                                        listRelease(&bind201);
                                                                        }
                                                                    else
                                                                        {
                                                                        }
                                                                    }
                                                                else
                                                                    {
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        else
                                            {
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        // ******** Start of Instrumentation 
        if (cmdCount == TEST_COUNT)
            {           
            end = millis();
            sendStringf("***TestEnd*** %d", end - start);
            }
        // ******** End of Instrumentation            
        }
    taskComplete();
    }
