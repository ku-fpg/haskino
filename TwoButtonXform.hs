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
    loopE $ rep <$> digitalReadE(abs(button1)) >>= 
            (\a -> rep <$> digitalReadE(abs(button2)) >>=
                (\b -> digitalWrite led (a || b)))

Apply "digitalWrite":
    loopE $ rep <$> digitalReadE(abs(button1)) >>= 
            (\a -> rep <$> digitalReadE(abs(button2)) >>=
                (\b -> digitalWriteE (abs(led)) (abs(a || b))))

Apply "abs-push-or"
    loopE $ rep <$> digitalReadE(abs(button1)) >>= 
            (\a -> rep <$> digitalReadE(abs(button2)) >>=
                (\b -> digitalWriteE (abs(led)) ((abs(a) || abs(b)))))

Apply "rep-3rd-monad":
    loopE $ digitalReadE(abs(button1)) >>= 
            (\a -> digitalReadE(abs(button2)) >>=
                (\b -> digitalWriteE (abs(led)) ((abs(a) ||* abs(b)))).rep).rep

Apply "????-1":
    loopE $ digitalReadE(abs(button1)) >>= 
            (\a' -> let a=rep(a') in digitalReadE(abs(button2)) >>=
                (\b' -> let b=rep(b') in digitalWriteE (abs(led)) ((abs(a) ||* abs(b)))))

Apply "????-2":
    loopE $ digitalReadE(abs(button1)) >>= 
            (\a' -> digitalReadE(abs(button2)) >>=
                (\b' -> digitalWriteE (abs(led)) ((abs(rep(a')) ||* abs(rep(b'))))))

Apply "abs-rep-fuse"
    loopE $ digitalReadE button1 >>= 
            \a' -> digitalReadE button2 >>=
                \b' -> digitalWriteE abs(led) (a' ||* b')
