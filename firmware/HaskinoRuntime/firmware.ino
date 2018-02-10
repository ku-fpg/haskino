#include "HaskinoRuntime.h"

void haskinoMain();
#define HASKINOMAIN_STACK_SIZE 792
byte haskinoMainTcb[sizeof(TCB) + HASKINOMAIN_STACK_SIZE];

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
    ref0 = 0;
    serialBegin(0,115200);
    uint8_t * bind2;
    uint8_t * bind3;
    while (1)
        {
        listAssign(&bind2, (uint8_t * ) (const byte[]) {255, 0});
        uint8_t bind5;
        while (1)
            {
            uint32_t bind6;
            while (1)
                {
                bind6 = serialAvailable(0);
                if ((0 < bind6))
                    {
                    int32_t bind7;

                    bind7 = serialRead(0);
                    bind5 = ((uint8_t) ((int32_t) bind7));
                    break;
                    }
                else
                    {
                    }
                }
            if ((bind5 == 125))
                {
                uint8_t bind9;
                uint8_t bind12;

                uint32_t bind10;
                while (1)
                    {
                    bind10 = serialAvailable(0);
                    if ((0 < bind10))
                        {
                        int32_t bind11;

                        bind11 = serialRead(0);
                        bind9 = ((uint8_t) ((int32_t) bind11));
                        break;
                        }
                    else
                        {
                        }
                    }
                bind12 = ref0;
                ref0 = ((bind12 + bind9) ^ 32);
                listAssign(&bind2, list8Cons((bind9 ^ 32),bind2));
                }
            else
                {
                if ((bind5 == 126))
                    {
                    uint8_t bind13;
                    uint8_t * bind14;

                    bind13 = ref0;
                    if ((bind13 == list8Elem(list8Reverse(bind2),(list8Len(list8Reverse(bind2)) - 1))))
                        {
                        bind14 = list8Slice(list8Reverse(bind2),0,(list8Len(list8Reverse(bind2)) - 1));
                        }
                    else
                        {
                        bind14 = (uint8_t * ) (const byte[]) {255, 0};
                        }
                    listAssign(&bind3, bind14);
                    break;
                    }
                else
                    {
                    listAssign(&bind2, list8Cons(bind5,bind2));
                    }
                listAssign(&bind2, list8Cons(bind5,bind2));
                }
            }
        if ((list8Len(bind3) == 0))
            {
            }
        else
            {
            if (((list8Elem(bind3,0) & 240) == 16))
                {
                if ((list8Elem(bind3,0) == 16))
                    {
                    }
                else
                    {
                    if ((list8Elem(bind3,0) == 17))
                        {
                        if (((list8Elem(list8Slice(bind3,1,0),0) == 2) && ((list8Elem(list8Slice(bind3,1,0),1) == 0) && ((list8Elem(list8Slice(bind3,1,0),3) == 2) && (list8Elem(list8Slice(bind3,1,0),4) == 0)))))
                            {
                            pinMode(list8Elem(list8Slice(bind3,1,0),2),(list8Elem(list8Slice(bind3,1,0),5) == 0) ? INPUT : (list8Elem(list8Slice(bind3,1,0),5) == 1) ? OUTPUT : (list8Elem(list8Slice(bind3,1,0),5) == 2) ? INPUT_PULLUP : INPUT);
                            }
                        else
                            {
                            }
                        }
                    else
                        {
                        if ((list8Elem(bind3,0) == 18))
                            {
                            if (((list8Elem(list8Slice(bind3,1,0),0) == 4) && (list8Elem(list8Slice(bind3,1,0),1) == 0)))
                                {
                                delayMilliseconds(((((((uint32_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),2))) << 24) | (((uint32_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),3))) << 16)) | (((uint32_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),4))) << 8)) | ((uint32_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),5)))));
                                }
                            else
                                {
                                }
                            }
                        else
                            {
                            if ((list8Elem(bind3,0) == 19))
                                {
                                if (((list8Elem(list8Slice(bind3,1,0),0) == 4) && (list8Elem(list8Slice(bind3,1,0),1) == 0)))
                                    {
                                    delayMicroseconds(((((((uint32_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),2))) << 24) | (((uint32_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),3))) << 16)) | (((uint32_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),4))) << 8)) | ((uint32_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),5)))));
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
                if (((list8Elem(bind3,0) & 240) == 32))
                    {
                    if ((list8Elem(bind3,0) == 32))
                        {
                        uint16_t bind26;
                        uint8_t * bind27;

                        bind26 = queryFirmware();
                        listAssign(&bind27, list8Cons(0,list8Cons(40,list8Cons(((uint8_t) ((int32_t) (bind26 >> 8))),list8Cons(((uint8_t) ((int32_t) (bind26 & 8))),(uint8_t * ) (const byte[]) {255, 0})))));
                        while (1)
                            {
                            if ((list8Len(bind27) == 2))
                                {
                                if ((((list8Elem(bind27,0) + list8Elem(bind27,1)) == 126) || ((list8Elem(bind27,0) + list8Elem(bind27,1)) == 125)))
                                    {
                                    serialWrite(0,125);
                                    serialWrite(0,((list8Elem(bind27,0) + list8Elem(bind27,1)) ^ 32));
                                    }
                                else
                                    {
                                    serialWrite(0,(list8Elem(bind27,0) + list8Elem(bind27,1)));
                                    }
                                break;
                                }
                            else
                                {
                                if (((list8Elem(bind27,0) == 126) || (list8Elem(bind27,0) == 125)))
                                    {
                                    serialWrite(0,125);
                                    serialWrite(0,(list8Elem(bind27,0) ^ 32));
                                    }
                                else
                                    {
                                    serialWrite(0,list8Elem(bind27,0));
                                    }
                                listAssign(&bind27, list8Cons((list8Elem(bind27,0) + list8Elem(bind27,1)),list8Slice(bind27,2,0)));
                                }
                            }
                        serialWrite(0,126);
                        }
                    else
                        {
                        if ((list8Elem(bind3,0) == 33))
                            {
                            uint8_t bind32;
                            uint8_t * bind33;

                            bind32 = queryProcessor();
                            listAssign(&bind33, list8Cons(0,list8Cons(41,list8Cons(bind32,(uint8_t * ) (const byte[]) {255, 0}))));
                            while (1)
                                {
                                if ((list8Len(bind33) == 2))
                                    {
                                    if ((((list8Elem(bind33,0) + list8Elem(bind33,1)) == 126) || ((list8Elem(bind33,0) + list8Elem(bind33,1)) == 125)))
                                        {
                                        serialWrite(0,125);
                                        serialWrite(0,((list8Elem(bind33,0) + list8Elem(bind33,1)) ^ 32));
                                        }
                                    else
                                        {
                                        serialWrite(0,(list8Elem(bind33,0) + list8Elem(bind33,1)));
                                        }
                                    break;
                                    }
                                else
                                    {
                                    if (((list8Elem(bind33,0) == 126) || (list8Elem(bind33,0) == 125)))
                                        {
                                        serialWrite(0,125);
                                        serialWrite(0,(list8Elem(bind33,0) ^ 32));
                                        }
                                    else
                                        {
                                        serialWrite(0,list8Elem(bind33,0));
                                        }
                                    listAssign(&bind33, list8Cons((list8Elem(bind33,0) + list8Elem(bind33,1)),list8Slice(bind33,2,0)));
                                    }
                                }
                            serialWrite(0,126);
                            }
                        else
                            {
                            if ((list8Elem(bind3,0) == 34))
                                {
                                uint32_t bind38;
                                uint8_t * bind39;

                                bind38 = micros();
                                listAssign(&bind39, list8Cons(0,list8Cons(42,list8Cons(4,list8Cons(0,list8Cons(((uint8_t) ((int32_t) (bind38 >> 24))),list8Cons(((uint8_t) ((int32_t) (bind38 >> 16))),list8Cons(((uint8_t) ((int32_t) (bind38 >> 8))),list8Cons(((uint8_t) ((int32_t) (bind38 & 8))),(uint8_t * ) (const byte[]) {255, 0})))))))));
                                while (1)
                                    {
                                    if ((list8Len(bind39) == 2))
                                        {
                                        if ((((list8Elem(bind39,0) + list8Elem(bind39,1)) == 126) || ((list8Elem(bind39,0) + list8Elem(bind39,1)) == 125)))
                                            {
                                            serialWrite(0,125);
                                            serialWrite(0,((list8Elem(bind39,0) + list8Elem(bind39,1)) ^ 32));
                                            }
                                        else
                                            {
                                            serialWrite(0,(list8Elem(bind39,0) + list8Elem(bind39,1)));
                                            }
                                        break;
                                        }
                                    else
                                        {
                                        if (((list8Elem(bind39,0) == 126) || (list8Elem(bind39,0) == 125)))
                                            {
                                            serialWrite(0,125);
                                            serialWrite(0,(list8Elem(bind39,0) ^ 32));
                                            }
                                        else
                                            {
                                            serialWrite(0,list8Elem(bind39,0));
                                            }
                                        listAssign(&bind39, list8Cons((list8Elem(bind39,0) + list8Elem(bind39,1)),list8Slice(bind39,2,0)));
                                        }
                                    }
                                serialWrite(0,126);
                                }
                            else
                                {
                                if ((list8Elem(bind3,0) == 35))
                                    {
                                    uint32_t bind44;
                                    uint8_t * bind45;

                                    bind44 = millis();
                                    listAssign(&bind45, list8Cons(0,list8Cons(42,list8Cons(4,list8Cons(0,list8Cons(((uint8_t) ((int32_t) (bind44 >> 24))),list8Cons(((uint8_t) ((int32_t) (bind44 >> 16))),list8Cons(((uint8_t) ((int32_t) (bind44 >> 8))),list8Cons(((uint8_t) ((int32_t) (bind44 & 8))),(uint8_t * ) (const byte[]) {255, 0})))))))));
                                    while (1)
                                        {
                                        if ((list8Len(bind45) == 2))
                                            {
                                            if ((((list8Elem(bind45,0) + list8Elem(bind45,1)) == 126) || ((list8Elem(bind45,0) + list8Elem(bind45,1)) == 125)))
                                                {
                                                serialWrite(0,125);
                                                serialWrite(0,((list8Elem(bind45,0) + list8Elem(bind45,1)) ^ 32));
                                                }
                                            else
                                                {
                                                serialWrite(0,(list8Elem(bind45,0) + list8Elem(bind45,1)));
                                                }
                                            break;
                                            }
                                        else
                                            {
                                            if (((list8Elem(bind45,0) == 126) || (list8Elem(bind45,0) == 125)))
                                                {
                                                serialWrite(0,125);
                                                serialWrite(0,(list8Elem(bind45,0) ^ 32));
                                                }
                                            else
                                                {
                                                serialWrite(0,list8Elem(bind45,0));
                                                }
                                            listAssign(&bind45, list8Cons((list8Elem(bind45,0) + list8Elem(bind45,1)),list8Slice(bind45,2,0)));
                                            }
                                        }
                                    serialWrite(0,126);
                                    }
                                else
                                    {
                                    if ((list8Elem(bind3,0) == 0))
                                        {
                                        uint8_t * bind50;

                                        listAssign(&bind50, list8Cons(0,list8Cons(44,list8Slice(bind3,1,0))));
                                        while (1)
                                            {
                                            if ((list8Len(bind50) == 2))
                                                {
                                                if ((((list8Elem(bind50,0) + list8Elem(bind50,1)) == 126) || ((list8Elem(bind50,0) + list8Elem(bind50,1)) == 125)))
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
                                                if (((list8Elem(bind50,0) == 126) || (list8Elem(bind50,0) == 125)))
                                                    {
                                                    serialWrite(0,125);
                                                    serialWrite(0,(list8Elem(bind50,0) ^ 32));
                                                    }
                                                else
                                                    {
                                                    serialWrite(0,list8Elem(bind50,0));
                                                    }
                                                listAssign(&bind50, list8Cons((list8Elem(bind50,0) + list8Elem(bind50,1)),list8Slice(bind50,2,0)));
                                                }
                                            }
                                        serialWrite(0,126);
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
                    if (((list8Elem(bind3,0) & 240) == 48))
                        {
                        if ((list8Elem(bind3,0) == 48))
                            {
                            if (((list8Elem(list8Slice(bind3,1,0),1) == 2) && (list8Elem(list8Slice(bind3,1,0),2) == 0)))
                                {
                                bool bind57;
                                uint8_t * bind58;

                                bind57 = digitalRead(list8Elem(list8Slice(bind3,1,0),3));
                                listAssign(&bind58, list8Cons(0,list8Cons(56,list8Cons(1,list8Cons(0,list8Cons(bind57 ? 1 : 0,(uint8_t * ) (const byte[]) {255, 0}))))));
                                while (1)
                                    {
                                    if ((list8Len(bind58) == 2))
                                        {
                                        if ((((list8Elem(bind58,0) + list8Elem(bind58,1)) == 126) || ((list8Elem(bind58,0) + list8Elem(bind58,1)) == 125)))
                                            {
                                            serialWrite(0,125);
                                            serialWrite(0,((list8Elem(bind58,0) + list8Elem(bind58,1)) ^ 32));
                                            }
                                        else
                                            {
                                            serialWrite(0,(list8Elem(bind58,0) + list8Elem(bind58,1)));
                                            }
                                        break;
                                        }
                                    else
                                        {
                                        if (((list8Elem(bind58,0) == 126) || (list8Elem(bind58,0) == 125)))
                                            {
                                            serialWrite(0,125);
                                            serialWrite(0,(list8Elem(bind58,0) ^ 32));
                                            }
                                        else
                                            {
                                            serialWrite(0,list8Elem(bind58,0));
                                            }
                                        listAssign(&bind58, list8Cons((list8Elem(bind58,0) + list8Elem(bind58,1)),list8Slice(bind58,2,0)));
                                        }
                                    }
                                serialWrite(0,126);
                                }
                            else
                                {
                                }
                            }
                        else
                            {
                            if ((list8Elem(bind3,0) == 49))
                                {
                                if (((list8Elem(list8Slice(bind3,1,0),0) == 2) && ((list8Elem(list8Slice(bind3,1,0),1) == 0) && ((list8Elem(list8Slice(bind3,1,0),3) == 2) && (list8Elem(list8Slice(bind3,1,0),4) == 0)))))
                                    {
                                    digitalWrite(list8Elem(list8Slice(bind3,1,0),2),(list8Elem(list8Slice(bind3,1,0),5) == 0) ? 0 : 1);
                                    }
                                else
                                    {
                                    }
                                }
                            else
                                {
                                if ((list8Elem(bind3,0) == 50))
                                    {
                                    if (((list8Elem(list8Slice(bind3,1,0),1) == 2) && ((list8Elem(list8Slice(bind3,1,0),2) == 0) && ((list8Elem(list8Slice(bind3,1,0),4) == 2) && (list8Elem(list8Slice(bind3,1,0),5) == 0)))))
                                        {
                                        uint8_t bind66;
                                        uint8_t * bind67;

                                        bind66 = digitalPortRead(list8Elem(list8Slice(bind3,1,0),3),list8Elem(list8Slice(bind3,1,0),6));
                                        listAssign(&bind67, list8Cons(0,list8Cons(57,list8Cons(2,list8Cons(0,list8Cons(bind66,(uint8_t * ) (const byte[]) {255, 0}))))));
                                        while (1)
                                            {
                                            if ((list8Len(bind67) == 2))
                                                {
                                                if ((((list8Elem(bind67,0) + list8Elem(bind67,1)) == 126) || ((list8Elem(bind67,0) + list8Elem(bind67,1)) == 125)))
                                                    {
                                                    serialWrite(0,125);
                                                    serialWrite(0,((list8Elem(bind67,0) + list8Elem(bind67,1)) ^ 32));
                                                    }
                                                else
                                                    {
                                                    serialWrite(0,(list8Elem(bind67,0) + list8Elem(bind67,1)));
                                                    }
                                                break;
                                                }
                                            else
                                                {
                                                if (((list8Elem(bind67,0) == 126) || (list8Elem(bind67,0) == 125)))
                                                    {
                                                    serialWrite(0,125);
                                                    serialWrite(0,(list8Elem(bind67,0) ^ 32));
                                                    }
                                                else
                                                    {
                                                    serialWrite(0,list8Elem(bind67,0));
                                                    }
                                                listAssign(&bind67, list8Cons((list8Elem(bind67,0) + list8Elem(bind67,1)),list8Slice(bind67,2,0)));
                                                }
                                            }
                                        serialWrite(0,126);
                                        }
                                    else
                                        {
                                        }
                                    }
                                else
                                    {
                                    if ((list8Elem(bind3,0) == 51))
                                        {
                                        if (((list8Elem(list8Slice(bind3,1,0),0) == 2) && ((list8Elem(list8Slice(bind3,1,0),1) == 0) && ((list8Elem(list8Slice(bind3,1,0),3) == 2) && ((list8Elem(list8Slice(bind3,1,0),4) == 0) && ((list8Elem(list8Slice(bind3,1,0),6) == 2) && (list8Elem(list8Slice(bind3,1,0),7) == 0)))))))
                                            {
                                            digitalPortWrite(list8Elem(list8Slice(bind3,1,0),2),list8Elem(list8Slice(bind3,1,0),5),list8Elem(list8Slice(bind3,1,0),8));
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
                        if (((list8Elem(bind3,0) & 240) == 64))
                            {
                            if ((list8Elem(bind3,0) == 64))
                                {
                                if (((list8Elem(list8Slice(bind3,1,0),1) == 2) && (list8Elem(list8Slice(bind3,1,0),2) == 0)))
                                    {
                                    uint16_t bind76;
                                    uint8_t * bind77;

                                    bind76 = analogRead(list8Elem(list8Slice(bind3,1,0),3));
                                    listAssign(&bind77, list8Cons(0,list8Cons(72,list8Cons(3,list8Cons(0,list8Cons(((uint8_t) ((int32_t) (bind76 >> 8))),list8Cons(((uint8_t) ((int32_t) (bind76 & 8))),(uint8_t * ) (const byte[]) {255, 0})))))));
                                    while (1)
                                        {
                                        if ((list8Len(bind77) == 2))
                                            {
                                            if ((((list8Elem(bind77,0) + list8Elem(bind77,1)) == 126) || ((list8Elem(bind77,0) + list8Elem(bind77,1)) == 125)))
                                                {
                                                serialWrite(0,125);
                                                serialWrite(0,((list8Elem(bind77,0) + list8Elem(bind77,1)) ^ 32));
                                                }
                                            else
                                                {
                                                serialWrite(0,(list8Elem(bind77,0) + list8Elem(bind77,1)));
                                                }
                                            break;
                                            }
                                        else
                                            {
                                            if (((list8Elem(bind77,0) == 126) || (list8Elem(bind77,0) == 125)))
                                                {
                                                serialWrite(0,125);
                                                serialWrite(0,(list8Elem(bind77,0) ^ 32));
                                                }
                                            else
                                                {
                                                serialWrite(0,list8Elem(bind77,0));
                                                }
                                            listAssign(&bind77, list8Cons((list8Elem(bind77,0) + list8Elem(bind77,1)),list8Slice(bind77,2,0)));
                                            }
                                        }
                                    serialWrite(0,126);
                                    }
                                else
                                    {
                                    }
                                }
                            else
                                {
                                if ((list8Elem(bind3,0) == 65))
                                    {
                                    if (((list8Elem(list8Slice(bind3,1,0),0) == 2) && ((list8Elem(list8Slice(bind3,1,0),1) == 0) && ((list8Elem(list8Slice(bind3,1,0),3) == 3) && (list8Elem(list8Slice(bind3,1,0),4) == 0)))))
                                        {
                                        analogWrite(list8Elem(list8Slice(bind3,1,0),2),((((uint16_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),5))) << 8) | ((uint16_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),6)))));
                                        }
                                    else
                                        {
                                        }
                                    }
                                else
                                    {
                                    if ((list8Elem(bind3,0) == 66))
                                        {
                                        if (((list8Elem(list8Slice(bind3,1,0),0) == 2) && ((list8Elem(list8Slice(bind3,1,0),1) == 0) && ((list8Elem(list8Slice(bind3,1,0),3) == 3) && ((list8Elem(list8Slice(bind3,1,0),4) == 0) && ((list8Elem(list8Slice(bind3,1,0),7) == 4) && (list8Elem(list8Slice(bind3,1,0),8) == 0)))))))
                                            {
                                            if ((((((((uint32_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),8))) << 24) | (((uint32_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),9))) << 16)) | (((uint32_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),10))) << 8)) | ((uint32_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),11)))) == 0))
                                                {
                                                tone(list8Elem(list8Slice(bind3,1,0),2),((((uint16_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),5))) << 8) | ((uint16_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),6)))),0);
                                                }
                                            else
                                                {
                                                tone(list8Elem(list8Slice(bind3,1,0),2),((((uint16_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),5))) << 8) | ((uint16_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),6)))),((((((uint32_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),8))) << 24) | (((uint32_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),9))) << 16)) | (((uint32_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),10))) << 8)) | ((uint32_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),11)))));
                                                }
                                            }
                                        else
                                            {
                                            }
                                        }
                                    else
                                        {
                                        if ((list8Elem(bind3,0) == 67))
                                            {
                                            if (((list8Elem(list8Slice(bind3,1,0),0) == 2) && (list8Elem(list8Slice(bind3,1,0),1) == 0)))
                                                {
                                                noTone(list8Elem(list8Slice(bind3,1,0),2));
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
                            if (((list8Elem(bind3,0) & 240) == 80))
                                {
                                if ((list8Elem(bind3,0) == 80))
                                    {
                                    i2cConfig();
                                    }
                                else
                                    {
                                    if ((list8Elem(bind3,0) == 81))
                                        {
                                        if (((list8Elem(list8Slice(bind3,1,0),1) == 2) && ((list8Elem(list8Slice(bind3,1,0),2) == 0) && ((list8Elem(list8Slice(bind3,1,0),4) == 2) && (list8Elem(list8Slice(bind3,1,0),5) == 0)))))
                                            {
                                            uint8_t * bind92 = NULL;
                                            uint8_t * bind93;

                                            listAssign(&bind92, i2cRead(list8Elem(list8Slice(bind3,1,0),3),list8Elem(list8Slice(bind3,1,0),6)));
                                            listAssign(&bind93, list8Cons(0,list8Cons(88,list8Cons(8,list8Cons(0,list8Cons(((uint8_t) list8Len(bind92)),bind92))))));
                                            while (1)
                                                {
                                                if ((list8Len(bind93) == 2))
                                                    {
                                                    if ((((list8Elem(bind93,0) + list8Elem(bind93,1)) == 126) || ((list8Elem(bind93,0) + list8Elem(bind93,1)) == 125)))
                                                        {
                                                        serialWrite(0,125);
                                                        serialWrite(0,((list8Elem(bind93,0) + list8Elem(bind93,1)) ^ 32));
                                                        }
                                                    else
                                                        {
                                                        serialWrite(0,(list8Elem(bind93,0) + list8Elem(bind93,1)));
                                                        }
                                                    break;
                                                    }
                                                else
                                                    {
                                                    if (((list8Elem(bind93,0) == 126) || (list8Elem(bind93,0) == 125)))
                                                        {
                                                        serialWrite(0,125);
                                                        serialWrite(0,(list8Elem(bind93,0) ^ 32));
                                                        }
                                                    else
                                                        {
                                                        serialWrite(0,list8Elem(bind93,0));
                                                        }
                                                    listAssign(&bind93, list8Cons((list8Elem(bind93,0) + list8Elem(bind93,1)),list8Slice(bind93,2,0)));
                                                    }
                                                }
                                            serialWrite(0,126);
                                            }
                                        else
                                            {
                                            }
                                        }
                                    else
                                        {
                                        if ((list8Elem(bind3,0) == 82))
                                            {
                                            if (((list8Elem(list8Slice(bind3,1,0),0) == 2) && ((list8Elem(list8Slice(bind3,1,0),1) == 0) && ((list8Elem(list8Slice(bind3,1,0),3) == 8) && ((list8Elem(list8Slice(bind3,1,0),4) == 0) && (list8Len(list8Slice(bind3,1,0)) == (((int32_t) list8Elem(list8Slice(bind3,1,0),5)) + 6)))))))
                                                {
                                                i2cWrite(list8Elem(list8Slice(bind3,1,0),2),list8Slice(list8Slice(bind3,1,0),6,0));
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
                                if (((list8Elem(bind3,0) & 240) == 224))
                                    {
                                    if ((list8Elem(bind3,0) == 224))
                                        {
                                        if (((list8Elem(list8Slice(bind3,1,0),0) == 2) && ((list8Elem(list8Slice(bind3,1,0),1) == 0) && ((list8Elem(list8Slice(bind3,1,0),3) == 4) && (list8Elem(list8Slice(bind3,1,0),4) == 0)))))
                                            {
                                            serialBegin(list8Elem(list8Slice(bind3,1,0),2),((((((uint32_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),5))) << 24) | (((uint32_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),6))) << 16)) | (((uint32_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),7))) << 8)) | ((uint32_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),8)))));
                                            }
                                        else
                                            {
                                            }
                                        }
                                    else
                                        {
                                        if ((list8Elem(bind3,0) == 225))
                                            {
                                            if (((list8Elem(list8Slice(bind3,1,0),0) == 2) && (list8Elem(list8Slice(bind3,1,0),1) == 0)))
                                                {
                                                serialEnd(list8Elem(list8Slice(bind3,1,0),2));
                                                }
                                            else
                                                {
                                                }
                                            }
                                        else
                                            {
                                            if ((list8Elem(bind3,0) == 227))
                                                {
                                                if (((list8Elem(list8Slice(bind3,1,0),1) == 2) && (list8Elem(list8Slice(bind3,1,0),2) == 0)))
                                                    {
                                                    int32_t bind106;
                                                    uint8_t * bind107;

                                                    bind106 = serialRead(list8Elem(list8Slice(bind3,1,0),3));
                                                    listAssign(&bind107, list8Cons(0,list8Cons(233,list8Cons(4,list8Cons(0,list8Cons(((uint8_t) ((int32_t) (bind106 >> 24))),list8Cons(((uint8_t) ((int32_t) (bind106 >> 16))),list8Cons(((uint8_t) ((int32_t) (bind106 >> 8))),list8Cons(((uint8_t) ((int32_t) (bind106 & 8))),(uint8_t * ) (const byte[]) {255, 0})))))))));
                                                    while (1)
                                                        {
                                                        if ((list8Len(bind107) == 2))
                                                            {
                                                            if ((((list8Elem(bind107,0) + list8Elem(bind107,1)) == 126) || ((list8Elem(bind107,0) + list8Elem(bind107,1)) == 125)))
                                                                {
                                                                serialWrite(0,125);
                                                                serialWrite(0,((list8Elem(bind107,0) + list8Elem(bind107,1)) ^ 32));
                                                                }
                                                            else
                                                                {
                                                                serialWrite(0,(list8Elem(bind107,0) + list8Elem(bind107,1)));
                                                                }
                                                            break;
                                                            }
                                                        else
                                                            {
                                                            if (((list8Elem(bind107,0) == 126) || (list8Elem(bind107,0) == 125)))
                                                                {
                                                                serialWrite(0,125);
                                                                serialWrite(0,(list8Elem(bind107,0) ^ 32));
                                                                }
                                                            else
                                                                {
                                                                serialWrite(0,list8Elem(bind107,0));
                                                                }
                                                            listAssign(&bind107, list8Cons((list8Elem(bind107,0) + list8Elem(bind107,1)),list8Slice(bind107,2,0)));
                                                            }
                                                        }
                                                    serialWrite(0,126);
                                                    }
                                                else
                                                    {
                                                    }
                                                }
                                            else
                                                {
                                                if ((list8Elem(bind3,0) == 228))
                                                    {
                                                    if (((list8Elem(list8Slice(bind3,1,0),1) == 2) && (list8Elem(list8Slice(bind3,1,0),2) == 0)))
                                                        {
                                                        uint8_t * bind113 = NULL;
                                                        uint8_t * bind114;

                                                        listAssign(&bind113, serialReadList(list8Elem(list8Slice(bind3,1,0),3)));
                                                        listAssign(&bind114, list8Cons(0,list8Cons(234,list8Cons(8,list8Cons(0,list8Cons(((uint8_t) list8Len(bind113)),bind113))))));
                                                        while (1)
                                                            {
                                                            if ((list8Len(bind114) == 2))
                                                                {
                                                                if ((((list8Elem(bind114,0) + list8Elem(bind114,1)) == 126) || ((list8Elem(bind114,0) + list8Elem(bind114,1)) == 125)))
                                                                    {
                                                                    serialWrite(0,125);
                                                                    serialWrite(0,((list8Elem(bind114,0) + list8Elem(bind114,1)) ^ 32));
                                                                    }
                                                                else
                                                                    {
                                                                    serialWrite(0,(list8Elem(bind114,0) + list8Elem(bind114,1)));
                                                                    }
                                                                break;
                                                                }
                                                            else
                                                                {
                                                                if (((list8Elem(bind114,0) == 126) || (list8Elem(bind114,0) == 125)))
                                                                    {
                                                                    serialWrite(0,125);
                                                                    serialWrite(0,(list8Elem(bind114,0) ^ 32));
                                                                    }
                                                                else
                                                                    {
                                                                    serialWrite(0,list8Elem(bind114,0));
                                                                    }
                                                                listAssign(&bind114, list8Cons((list8Elem(bind114,0) + list8Elem(bind114,1)),list8Slice(bind114,2,0)));
                                                                }
                                                            }
                                                        serialWrite(0,126);
                                                        }
                                                    else
                                                        {
                                                        }
                                                    }
                                                else
                                                    {
                                                    if ((list8Elem(bind3,0) == 229))
                                                        {
                                                        if (((list8Elem(list8Slice(bind3,1,0),0) == 2) && ((list8Elem(list8Slice(bind3,1,0),1) == 0) && ((list8Elem(list8Slice(bind3,1,0),3) == 2) && (list8Elem(list8Slice(bind3,1,0),4) == 0)))))
                                                            {
                                                            serialWrite(list8Elem(list8Slice(bind3,1,0),2),list8Elem(list8Slice(bind3,1,0),5));
                                                            }
                                                        else
                                                            {
                                                            }
                                                        }
                                                    else
                                                        {
                                                        if ((list8Elem(bind3,0) == 230))
                                                            {
                                                            if (((list8Elem(list8Slice(bind3,1,0),0) == 2) && ((list8Elem(list8Slice(bind3,1,0),1) == 0) && ((list8Elem(list8Slice(bind3,1,0),3) == 8) && ((list8Elem(list8Slice(bind3,1,0),4) == 0) && (list8Len(list8Slice(bind3,1,0)) == (((int32_t) list8Elem(list8Slice(bind3,1,0),5)) + 6)))))))
                                                                {
                                                                serialWriteList(list8Elem(list8Slice(bind3,1,0),2),list8Slice(list8Slice(bind3,1,0),6,0));
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
                                    if (((list8Elem(bind3,0) & 240) == 96))
                                        {
                                        if ((list8Elem(bind3,0) == 96))
                                            {
                                            if (((list8Elem(list8Slice(bind3,1,0),1) == 3) && ((list8Elem(list8Slice(bind3,1,0),2) == 0) && ((list8Elem(list8Slice(bind3,1,0),5) == 2) && ((list8Elem(list8Slice(bind3,1,0),6) == 0) && ((list8Elem(list8Slice(bind3,1,0),8) == 2) && (list8Elem(list8Slice(bind3,1,0),9) == 0)))))))
                                                {
                                                uint8_t bind125;
                                                uint8_t * bind126;

                                                bind125 = stepper2Pin(((((uint16_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),3))) << 8) | ((uint16_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),4)))),list8Elem(list8Slice(bind3,1,0),7),list8Elem(list8Slice(bind3,1,0),10));
                                                listAssign(&bind126, list8Cons(0,list8Cons(104,list8Cons(2,list8Cons(0,list8Cons(bind125,(uint8_t * ) (const byte[]) {255, 0}))))));
                                                while (1)
                                                    {
                                                    if ((list8Len(bind126) == 2))
                                                        {
                                                        if ((((list8Elem(bind126,0) + list8Elem(bind126,1)) == 126) || ((list8Elem(bind126,0) + list8Elem(bind126,1)) == 125)))
                                                            {
                                                            serialWrite(0,125);
                                                            serialWrite(0,((list8Elem(bind126,0) + list8Elem(bind126,1)) ^ 32));
                                                            }
                                                        else
                                                            {
                                                            serialWrite(0,(list8Elem(bind126,0) + list8Elem(bind126,1)));
                                                            }
                                                        break;
                                                        }
                                                    else
                                                        {
                                                        if (((list8Elem(bind126,0) == 126) || (list8Elem(bind126,0) == 125)))
                                                            {
                                                            serialWrite(0,125);
                                                            serialWrite(0,(list8Elem(bind126,0) ^ 32));
                                                            }
                                                        else
                                                            {
                                                            serialWrite(0,list8Elem(bind126,0));
                                                            }
                                                        listAssign(&bind126, list8Cons((list8Elem(bind126,0) + list8Elem(bind126,1)),list8Slice(bind126,2,0)));
                                                        }
                                                    }
                                                serialWrite(0,126);
                                                }
                                            else
                                                {
                                                }
                                            }
                                        else
                                            {
                                            if ((list8Elem(bind3,0) == 97))
                                                {
                                                if (((list8Elem(list8Slice(bind3,1,0),1) == 3) && ((list8Elem(list8Slice(bind3,1,0),2) == 0) && ((list8Elem(list8Slice(bind3,1,0),5) == 2) && ((list8Elem(list8Slice(bind3,1,0),6) == 0) && ((list8Elem(list8Slice(bind3,1,0),8) == 2) && ((list8Elem(list8Slice(bind3,1,0),9) == 0) && ((list8Elem(list8Slice(bind3,1,0),11) == 2) && ((list8Elem(list8Slice(bind3,1,0),12) == 0) && ((list8Elem(list8Slice(bind3,1,0),14) == 2) && (list8Elem(list8Slice(bind3,1,0),15) == 0)))))))))))
                                                    {
                                                    uint8_t bind132;
                                                    uint8_t * bind133;

                                                    bind132 = stepper4Pin(((((((uint16_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),3))) << 24) | (((uint16_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),4))) << 16)) | (((uint16_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),5))) << 8)) | ((uint16_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),6)))),list8Elem(list8Slice(bind3,1,0),7),list8Elem(list8Slice(bind3,1,0),10),list8Elem(list8Slice(bind3,1,0),13),list8Elem(list8Slice(bind3,1,0),16));
                                                    listAssign(&bind133, list8Cons(0,list8Cons(104,list8Cons(2,list8Cons(0,list8Cons(bind132,(uint8_t * ) (const byte[]) {255, 0}))))));
                                                    while (1)
                                                        {
                                                        if ((list8Len(bind133) == 2))
                                                            {
                                                            if ((((list8Elem(bind133,0) + list8Elem(bind133,1)) == 126) || ((list8Elem(bind133,0) + list8Elem(bind133,1)) == 125)))
                                                                {
                                                                serialWrite(0,125);
                                                                serialWrite(0,((list8Elem(bind133,0) + list8Elem(bind133,1)) ^ 32));
                                                                }
                                                            else
                                                                {
                                                                serialWrite(0,(list8Elem(bind133,0) + list8Elem(bind133,1)));
                                                                }
                                                            break;
                                                            }
                                                        else
                                                            {
                                                            if (((list8Elem(bind133,0) == 126) || (list8Elem(bind133,0) == 125)))
                                                                {
                                                                serialWrite(0,125);
                                                                serialWrite(0,(list8Elem(bind133,0) ^ 32));
                                                                }
                                                            else
                                                                {
                                                                serialWrite(0,list8Elem(bind133,0));
                                                                }
                                                            listAssign(&bind133, list8Cons((list8Elem(bind133,0) + list8Elem(bind133,1)),list8Slice(bind133,2,0)));
                                                            }
                                                        }
                                                    serialWrite(0,126);
                                                    }
                                                else
                                                    {
                                                    }
                                                }
                                            else
                                                {
                                                if ((list8Elem(bind3,0) == 98))
                                                    {
                                                    if (((list8Elem(list8Slice(bind3,1,0),0) == 2) && ((list8Elem(list8Slice(bind3,1,0),1) == 0) && ((list8Elem(list8Slice(bind3,1,0),3) == 4) && (list8Elem(list8Slice(bind3,1,0),4) == 0)))))
                                                        {
                                                        stepperSetSpeed(list8Elem(list8Slice(bind3,1,0),2),((((((int32_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),5))) << 24) | (((int32_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),6))) << 16)) | (((int32_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),7))) << 8)) | ((int32_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),8)))));
                                                        }
                                                    else
                                                        {
                                                        }
                                                    }
                                                else
                                                    {
                                                    if ((list8Elem(bind3,0) == 99))
                                                        {
                                                        if (((list8Elem(list8Slice(bind3,1,0),1) == 2) && ((list8Elem(list8Slice(bind3,1,0),2) == 0) && ((list8Elem(list8Slice(bind3,1,0),4) == 3) && (list8Elem(list8Slice(bind3,1,0),5) == 0)))))
                                                            {
                                                            uint8_t * bind141;

                                                            stepperSetSpeed(list8Elem(list8Slice(bind3,1,0),3),((((int32_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),6))) << 8) | ((int32_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),7)))));
                                                            listAssign(&bind141, list8Cons(0,list8Cons(106,(uint8_t * ) (const byte[]) {255, 0})));
                                                            while (1)
                                                                {
                                                                if ((list8Len(bind141) == 2))
                                                                    {
                                                                    if ((((list8Elem(bind141,0) + list8Elem(bind141,1)) == 126) || ((list8Elem(bind141,0) + list8Elem(bind141,1)) == 125)))
                                                                        {
                                                                        serialWrite(0,125);
                                                                        serialWrite(0,((list8Elem(bind141,0) + list8Elem(bind141,1)) ^ 32));
                                                                        }
                                                                    else
                                                                        {
                                                                        serialWrite(0,(list8Elem(bind141,0) + list8Elem(bind141,1)));
                                                                        }
                                                                    break;
                                                                    }
                                                                else
                                                                    {
                                                                    if (((list8Elem(bind141,0) == 126) || (list8Elem(bind141,0) == 125)))
                                                                        {
                                                                        serialWrite(0,125);
                                                                        serialWrite(0,(list8Elem(bind141,0) ^ 32));
                                                                        }
                                                                    else
                                                                        {
                                                                        serialWrite(0,list8Elem(bind141,0));
                                                                        }
                                                                    listAssign(&bind141, list8Cons((list8Elem(bind141,0) + list8Elem(bind141,1)),list8Slice(bind141,2,0)));
                                                                    }
                                                                }
                                                            serialWrite(0,126);
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
                                        if (((list8Elem(bind3,0) & 240) == 128))
                                            {
                                            if ((list8Elem(bind3,0) == 128))
                                                {
                                                if (((list8Elem(list8Slice(bind3,1,0),1) == 2) && ((list8Elem(list8Slice(bind3,1,0),2) == 0) && ((list8Elem(list8Slice(bind3,1,0),4) == 3) && ((list8Elem(list8Slice(bind3,1,0),5) == 0) && ((list8Elem(list8Slice(bind3,1,0),8) == 3) && (list8Elem(list8Slice(bind3,1,0),9) == 0)))))))
                                                    {
                                                    uint8_t bind148;
                                                    uint8_t * bind149;

                                                    bind148 = servoAttachMinMax(list8Elem(list8Slice(bind3,1,0),3),((((int16_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),6))) << 8) | ((int16_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),7)))),((((int16_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),10))) << 8) | ((int16_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),11)))));
                                                    listAssign(&bind149, list8Cons(0,list8Cons(136,list8Cons(2,list8Cons(0,list8Cons(bind148,(uint8_t * ) (const byte[]) {255, 0}))))));
                                                    while (1)
                                                        {
                                                        if ((list8Len(bind149) == 2))
                                                            {
                                                            if ((((list8Elem(bind149,0) + list8Elem(bind149,1)) == 126) || ((list8Elem(bind149,0) + list8Elem(bind149,1)) == 125)))
                                                                {
                                                                serialWrite(0,125);
                                                                serialWrite(0,((list8Elem(bind149,0) + list8Elem(bind149,1)) ^ 32));
                                                                }
                                                            else
                                                                {
                                                                serialWrite(0,(list8Elem(bind149,0) + list8Elem(bind149,1)));
                                                                }
                                                            break;
                                                            }
                                                        else
                                                            {
                                                            if (((list8Elem(bind149,0) == 126) || (list8Elem(bind149,0) == 125)))
                                                                {
                                                                serialWrite(0,125);
                                                                serialWrite(0,(list8Elem(bind149,0) ^ 32));
                                                                }
                                                            else
                                                                {
                                                                serialWrite(0,list8Elem(bind149,0));
                                                                }
                                                            listAssign(&bind149, list8Cons((list8Elem(bind149,0) + list8Elem(bind149,1)),list8Slice(bind149,2,0)));
                                                            }
                                                        }
                                                    serialWrite(0,126);
                                                    }
                                                else
                                                    {
                                                    }
                                                }
                                            else
                                                {
                                                if ((list8Elem(bind3,0) == 129))
                                                    {
                                                    if (((list8Elem(list8Slice(bind3,1,0),0) == 2) && (list8Elem(list8Slice(bind3,1,0),1) == 0)))
                                                        {
                                                        servoDetach(list8Elem(list8Slice(bind3,1,0),2));
                                                        }
                                                    else
                                                        {
                                                        }
                                                    }
                                                else
                                                    {
                                                    if ((list8Elem(bind3,0) == 130))
                                                        {
                                                        if (((list8Elem(list8Slice(bind3,1,0),0) == 2) && ((list8Elem(list8Slice(bind3,1,0),1) == 0) && ((list8Elem(list8Slice(bind3,1,0),3) == 3) && (list8Elem(list8Slice(bind3,1,0),4) == 0)))))
                                                            {
                                                            servoWrite(list8Elem(list8Slice(bind3,1,0),2),((((int16_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),5))) << 8) | ((int16_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),6)))));
                                                            }
                                                        else
                                                            {
                                                            }
                                                        }
                                                    else
                                                        {
                                                        if ((list8Elem(bind3,0) == 131))
                                                            {
                                                            if (((list8Elem(list8Slice(bind3,1,0),0) == 2) && ((list8Elem(list8Slice(bind3,1,0),1) == 0) && ((list8Elem(list8Slice(bind3,1,0),3) == 3) && (list8Elem(list8Slice(bind3,1,0),4) == 0)))))
                                                                {
                                                                servoWrite(list8Elem(list8Slice(bind3,1,0),2),((((int16_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),5))) << 8) | ((int16_t) ((int32_t) list8Elem(list8Slice(bind3,1,0),6)))));
                                                                }
                                                            else
                                                                {
                                                                }
                                                            }
                                                        else
                                                            {
                                                            if ((list8Elem(bind3,0) == 132))
                                                                {
                                                                if (((list8Elem(list8Slice(bind3,1,0),1) == 2) && (list8Elem(list8Slice(bind3,1,0),2) == 0)))
                                                                    {
                                                                    uint16_t bind161;
                                                                    uint8_t * bind162;

                                                                    bind161 = servoRead(list8Elem(list8Slice(bind3,1,0),3));
                                                                    listAssign(&bind162, list8Cons(0,list8Cons(137,list8Cons(3,list8Cons(0,list8Cons(((uint8_t) ((int32_t) (bind161 >> 8))),list8Cons(((uint8_t) ((int32_t) (bind161 & 8))),(uint8_t * ) (const byte[]) {255, 0})))))));
                                                                    while (1)
                                                                        {
                                                                        if ((list8Len(bind162) == 2))
                                                                            {
                                                                            if ((((list8Elem(bind162,0) + list8Elem(bind162,1)) == 126) || ((list8Elem(bind162,0) + list8Elem(bind162,1)) == 125)))
                                                                                {
                                                                                serialWrite(0,125);
                                                                                serialWrite(0,((list8Elem(bind162,0) + list8Elem(bind162,1)) ^ 32));
                                                                                }
                                                                            else
                                                                                {
                                                                                serialWrite(0,(list8Elem(bind162,0) + list8Elem(bind162,1)));
                                                                                }
                                                                            break;
                                                                            }
                                                                        else
                                                                            {
                                                                            if (((list8Elem(bind162,0) == 126) || (list8Elem(bind162,0) == 125)))
                                                                                {
                                                                                serialWrite(0,125);
                                                                                serialWrite(0,(list8Elem(bind162,0) ^ 32));
                                                                                }
                                                                            else
                                                                                {
                                                                                serialWrite(0,list8Elem(bind162,0));
                                                                                }
                                                                            listAssign(&bind162, list8Cons((list8Elem(bind162,0) + list8Elem(bind162,1)),list8Slice(bind162,2,0)));
                                                                            }
                                                                        }
                                                                    serialWrite(0,126);
                                                                    }
                                                                else
                                                                    {
                                                                    }
                                                                }
                                                            else
                                                                {
                                                                if ((list8Elem(bind3,0) == 133))
                                                                    {
                                                                    if (((list8Elem(list8Slice(bind3,1,0),1) == 2) && (list8Elem(list8Slice(bind3,1,0),2) == 0)))
                                                                        {
                                                                        uint16_t bind168;
                                                                        uint8_t * bind169;

                                                                        bind168 = servoReadMicros(list8Elem(list8Slice(bind3,1,0),3));
                                                                        listAssign(&bind169, list8Cons(0,list8Cons(137,list8Cons(3,list8Cons(0,list8Cons(((uint8_t) ((int32_t) (bind168 >> 8))),list8Cons(((uint8_t) ((int32_t) (bind168 & 8))),(uint8_t * ) (const byte[]) {255, 0})))))));
                                                                        while (1)
                                                                            {
                                                                            if ((list8Len(bind169) == 2))
                                                                                {
                                                                                if ((((list8Elem(bind169,0) + list8Elem(bind169,1)) == 126) || ((list8Elem(bind169,0) + list8Elem(bind169,1)) == 125)))
                                                                                    {
                                                                                    serialWrite(0,125);
                                                                                    serialWrite(0,((list8Elem(bind169,0) + list8Elem(bind169,1)) ^ 32));
                                                                                    }
                                                                                else
                                                                                    {
                                                                                    serialWrite(0,(list8Elem(bind169,0) + list8Elem(bind169,1)));
                                                                                    }
                                                                                break;
                                                                                }
                                                                            else
                                                                                {
                                                                                if (((list8Elem(bind169,0) == 126) || (list8Elem(bind169,0) == 125)))
                                                                                    {
                                                                                    serialWrite(0,125);
                                                                                    serialWrite(0,(list8Elem(bind169,0) ^ 32));
                                                                                    }
                                                                                else
                                                                                    {
                                                                                    serialWrite(0,list8Elem(bind169,0));
                                                                                    }
                                                                                listAssign(&bind169, list8Cons((list8Elem(bind169,0) + list8Elem(bind169,1)),list8Slice(bind169,2,0)));
                                                                                }
                                                                            }
                                                                        serialWrite(0,126);
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
        }
    taskComplete();
    }
