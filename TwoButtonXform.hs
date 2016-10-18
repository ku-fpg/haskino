    loop $ do 
        a <- digitalRead button1
        b <- digitalRead button2
        digitalWrite led (a || b)

Desugar:

    loop $ digitalRead button1 >>= 
            /a -> digitalRead button2 >>=
                /b -> digitalWrite led (a || b)

Apply "loop":

    loopE $ digitalRead button1 >>= 
            /a -> digitalRead button2 >>=
                /b -> digitalWrite led (a || b)

Apply "digitalRead":

    loopE $ liftM(repB) $ digitalReadE button1 >>= 
            /a -> liftM(repB) $ digitalReadE button2 >>=
                /b -> digitalWrite led (a || b)

Apply "digitalWrite":

    loopE $ liftM(repB) $ digitalReadE button1 >>= 
            /a -> liftM(repB) $ digitalReadE button2 >>=
                /b -> digitalWriteE abs8(led) absB(a || b)

Apply "abs-push-or"

    loopE $ liftM(repB) $ digitalReadE button1 >>= 
            /a -> liftM(repB) $ digitalReadE button2 >>=
                /b -> digitalWriteE abs8(led) (absB(a) ||* absB(b))

Somehow float absB's outside lambdas:

    loopE $ liftM(absB) $ liftM(repB) $ digitalReadE button1 >>= 
            /a -> liftM(absB) $ liftM(repB) $ digitalReadE button2 >>=
                /b -> digitalWriteE abs8(led) (a ||* b)

Apply "elimM-repB-absB"

    loopE $ digitalReadE button1 >>= 
            /a -> digitalReadE button2 >>=
                /b -> digitalWriteE abs8(led) (a ||* b)


