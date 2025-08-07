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

LIST

RUN

NEW
int i = 10
func pr()
    i = i + 1
    print i
endfunc

pr() : pr()

pr() : pr() : pr()




FUNC Fibonacci(n)
    IF n <= 1 THEN 
        RETURN n
    ENDIF
    RETURN Fibonacci(n-1) + Fibonacci(n-2)
ENDFUNC

BEGIN
    PRINT Fibonacci(10) : ' Should print 55
END




BEGIN
    PRINT "Hello World"
END

BEGIN
    PRINT "A", "B", "C"
END

BEGIN
    PRINT "X"; "Y"; "Z"
END

BEGIN
    PRINT "Name:", "John"; " Age:", 25
END

BEGIN
    PRINT "Loading",
    PRINT "Done"
END

BEGIN
    PRINT "Progress: ";
    PRINT "100%"
END

INT x = 10
INT y = 20
BEGIN
    PRINT "x =", x, "y =", y
END

STRING name = "Fibonacci"
INT arg = 10
INT result = 55
BEGIN
    PRINT name; "("; arg; ") = "; result
END

BEGIN
    PRINT "Result:", 5 + 3 * 2; " (calculated)"
END

BEGIN
    PRINT
    PRINT "Empty line above"
    PRINT
END

BEGIN
    PRINT "A",
    PRINT "B",
    PRINT "C"
END

BEGIN
    PRINT "Multi"; "line"; "output"
    PRINT "Second", "line", "here"
END

BIT flag = TRUE
BEGIN
    PRINT "Flag is", flag
END

WORD count = 42
BYTE value = 255
BEGIN
    PRINT count; "/"; value
END

BEGIN
    PRINT "Start";
    PRINT;
    PRINT "End"
END



PRINT "Hello World"

PRINT "A", "B", "C"

PRINT "X"; "Y"; "Z"

PRINT "Name:", "John"; " Age:", 25

PRINT "Loading",

PRINT "Progress: ";

INT x = 10
INT y = 20
PRINT "x =", x, "y =", y

STRING name = "Fibonacci"
INT arg = 10
INT result = 55
PRINT name; "("; arg; ") = "; result

PRINT "Result:", 5 + 3 * 2; " (calculated)"

PRINT

PRINT "A",

PRINT "Multi"; "line"; "output"

BIT flag = TRUE
PRINT "Flag is", flag

WORD count = 42
BYTE value = 255
PRINT count; "/"; value

PRINT "Start";


INT I = 0
BEGIN
    IF  (I = 10) THEN
        PRINT "FALSE"
    ENDIF
    IF  (I <> 10) THEN
        PRINT "TRUE"
    ENDIF
    IF  (I <> 10) THEN
        PRINT "TRUE"
    ELSE
        PRINT "FALSE"
    ENDIF
    IF  (I = 10) THEN
        PRINT "FALSE"
    ELSE
        PRINT "TRUE"
    ENDIF
    
END
RUN

