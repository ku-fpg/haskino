    loop $ do 
        a <- digitalRead button1
        b <- digitalRead button2
        digitalWrite led (a || b)

Desugar:

    loop $ digitalRead button1 >>= 
            \a -> digitalRead button2 >>=
                \b -> digitalWrite led (a || b)

Apply "loop":

    loopE $ digitalRead button1 >>= 
            \a -> digitalRead button2 >>=
                \b -> digitalWrite led (a || b)

Apply "digitalRead":

    loopE $ repB <$> digitalReadE(abs8(button1)) >>= 
            (\a -> repB <$> digitalReadE(abs8(button2)) >>=
                (\b -> digitalWrite led (a || b)))

Apply "digitalWrite":

    loopE $ repB <$> digitalReadE(abs8(button1)) >>= 
            (\a -> repB <$> digitalReadE(abs8(button2)) >>=
                (\b -> digitalWriteE (abs8(led)) (absB(a || b))))

Apply "abs-push-or"

    loopE $ repB <$> digitalReadE(abs8(button1)) >>= 
            (\a -> repB <$> digitalReadE(abs8(button2)) >>=
                (\b -> digitalWriteE (abs8(led)) ((absB(a) || absB(b)))))

Apply "repB-3rd-monad":

    loopE $ digitalReadE(abs8(button1)) >>= 
            (\a -> digitalReadE(abs8(button2)) >>=
                (\b -> digitalWriteE (abs8(led)) ((absB(a) ||* absB(b)))).repB).repB

Apply "fuse-digitalWriteE" Rules

    loopE $ digitalReadE button1 >>= 
            \a -> digitalReadE button2 >>=
                \b -> digitalWriteE abs8(led) (a ||* b)


