WORD ELAPSED
WORD I = 0

FUNC ONE()
    PRINT "ONE"
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

LIST

RUN




NEW

PRINT "Hello"; : PRINT "World";
PRINT "Hello"; : PRINT "World";



STRING FOO = "Hello"
BEGIN
    PRINT FOO
END
RUN
VARS
DASM


FUNC FOO()
    PRINT "Hello";
    PRINT "World";
ENDFUNC
FOO();
FOO();

BEGIN
    PRINT "Hello";
    PRINT "World";
END
RUN
RUN

DASM


STRING SPACER = "Space"

RUN
RUN

DASM





NEW
int i = 10

func ppr()
    print i
endfunc
func pr()
    i = i + 1
    ppr()
endfunc

heap

pr()




INT I = 0
BEGIN
    IF (I = 10) THEN
        PRINT "FALSE"
    ENDIF
    IF (I <> 10) THEN
        PRINT "TRUE"
    ENDIF
    IF (I <> 10) THEN
        PRINT "TRUE"
    ELSE
        PRINT "FALSE"
    ENDIF
    IF (I = 10) THEN
        PRINT "FALSE"
    ELSE
        PRINT "TRUE"
    ENDIF
END
RUN

INT I = 0
BEGIN
    IF (I <> 10) THEN
        PRINT "IF"
    ELSE
        PRINT "ELSE"
    ENDIF
END
RUN


INT I = 0
BEGIN
    DO
        I = I + 1
        PRINT I
    UNTIL I = 10
    PRINT "OK"
END
RUN

BEGIN
    FOR I = 1 TO 10
        PRINT I
    NEXT I
    FOR I = 1 TO 10 STEP 2
        PRINT I
    NEXT I
    FOR I = 10 TO 1 STEP -2
        PRINT I
    NEXT I
END

TRON






string STR = "value"
FUNC FOO(C,S)
    WORD B = C
    PRINT B
    PRINT S
ENDFUNC
BEGIN
    STR = "Hello World"
    INT A = 1000
    FOO(A,STR)
END
RUN






FUNC FOO(C)
    INT B = C
    RETURN 10 * B
ENDFUNC

BEGIN
    INT A
    A = 5
    PRINT A
    PRINT FOO(A)
END
RUN
DASM

BEGIN
    IF (1 <= 1) THEN
        PRINT "HERE"
    ENDIF
    PRINT "PASSED"
END


BEGIN
    BYTE I
    FOR I = -5 TO 5
        PRINT I
    NEXT I
END
RUN
DASM



INT I
BEGIN
    FOR I = 1 TO 10
        PRINT I
    NEXT I
END
RUN

DASM
VARS


BEGIN
    WORD J
    STRING STR = "HELLO"
    FOR J = 1 TO 10
        PRINT J
    NEXT J
    PRINT STR
END
RUN

BEGIN
    FOR I = 1 TO 10 STEP 2
        PRINT I
    NEXT I
END

BEGIN
    FOR I = 10 TO 0 STEP -2
        PRINT I
    NEXT I
END

BEGIN
    INT J
    FOR I = 1 TO 10
        FOR J = 1 TO 10
            PRINT "J:", J
        NEXT J
        PRINT "I", I
    NEXT I
END
RUN






FUNC Fibo(n)
    IF n <= 1 THEN
        RETURN n
    ENDIF
    RETURN Fibo(n-1) + Fibo(n-2)
ENDFUNC

BEGIN
    WORD ELAPSED = SECONDS()
    PRINT FIBO(5)
    PRINT SECONDS()-ELAPSED;
    PRINT " seconds"
END
RUN


VAR S
VAR J
VAR I
BEGIN
    VAR START = SECONDS()
    FOR I = 1 TO 10
        S = 0
        FOR J = 1 TO 1000
            S = S + J
        NEXT J
        PRINT ".";
    NEXT I
    PRINT S
    PRINT SECONDS() - START; " seconds"
END
RUN

WORD S
WORD J
WORD I
BEGIN
    WORD START = SECONDS()
    FOR I = 1 TO 10
        S = 0
        FOR J = 1 TO 1000
            S = S + J
        NEXT J
        PRINT ".";
    NEXT I
    PRINT S
    PRINT SECONDS() - START; " seconds"
END
RUN


BEGIN
    VAR START = SECONDS()
    VAR S
    VAR J
    FOR I = 1 TO 10
        S = 0
        FOR J = 1 TO 1000
            S = S + J
        NEXT J
        PRINT ".";
    NEXT I
    PRINT S
    PRINT SECONDS() - START; " seconds"
END
RUN

BEGIN
    VAR START = SECONDS()
    VAR S
    VAR J
    VAR I
    FOR  I = 1 TO 10
        S = 0
        FOR  J = 1 TO 1000
            S = S + J
        NEXT J
        PRINT ".";
    NEXT I
    PRINT S
    PRINT SECONDS() - START; " seconds"
END
RUN


FUNC FOO()
PRINT 42
ENDFUNC

FOO()
DASM
HEAP

FUNC BOO()
FOO()
ENDFUNC

FUNC BOO()
FOO()
ENDFUNC



DASM
HEAP
