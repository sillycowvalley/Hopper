WORD ELAPSED
WORD I = 0

FUNC ONE()
    RETURN 1
ENDFUNC

FUNC FOO()
    WHILE (I < 10)
        PRINT I
        I = I + ONE()
        DELAY(1000)
    WEND
    PRINT "SUCCESS"
    RETURN 1 + 1
ENDFUNC

BEGIN
    ELAPSED = SECONDS()
    FOO()
    ELAPSED = SECONDS() - ELAPSED
    PRINT "Elapsed: "
    PRINT ELAPSED
END

RUN

FORGET FOO

HEAP




FUNC Fibonacci(n)
    IF n <= 1 THEN 
        RETURN n
    ENDIF
    RETURN Fibonacci(n-1) + Fibonacci(n-2)
ENDFUNC

BEGIN
    PRINT Fibonacci(10) : ' Should print 55
END