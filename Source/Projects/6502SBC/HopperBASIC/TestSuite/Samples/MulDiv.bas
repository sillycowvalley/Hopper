NEW
CLS
MEM

FUNC MUL()
    PRINT "WORD *: ";
    WORD A = 0x55
    WORD B = 0xAA
    WORD C
    FOR I = 1 TO 1000
        C = A * B
    NEXT I
ENDFUNC

FUNC DIV()
    PRINT "WORD /: ";
    WORD C = 0x3872
    WORD A = 0x55
    WORD B = 0xAA
    
    FOR I = 1 TO 1000
        A = C / B
    NEXT I
ENDFUNC

FUNC MULI()
    PRINT "INT *: ";
    INT A = 0x55
    INT B = 0xAA
    INT C
    FOR I = 1 TO 1000
        C = A * B
    NEXT I
ENDFUNC

FUNC DIVI()
    PRINT "INT /: ";
    INT C = 0x3872
    INT A = 0x55
    INT B = 0xAA
    
    FOR I = 1 TO 1000
        A = C / B
    NEXT I
ENDFUNC

FUNC MULV()
    PRINT "VAR *: ";
    VAR A = 0x55
    VAR B = 0xAA
    VAR C
    FOR I = 1 TO 1000
        C = A * B
    NEXT I
ENDFUNC

FUNC DIVV()
    PRINT "VAR /: ";
    VAR C = 0x3872
    VAR A = 0x55
    VAR B = 0xAA
    
    FOR I = 1 TO 1000
        A = C / B
    NEXT I
ENDFUNC



FUNC MULL()
    PRINT "LONG *: ";
    LONG A = 0x55
    LONG B = 0xAA
    LONG C
    FOR I = 1 TO 1000
        C = A * B
    NEXT I
ENDFUNC

FUNC DIVL()
    PRINT "LONG /: ";
    LONG C = 0x3872
    LONG A = 0x55
    LONG B = 0xAA
    
    FOR I = 1 TO 1000
        A = C / B
    NEXT I
ENDFUNC


BEGIN
    LONG START = MILLIS()
    MUL()
    PRINT MILLIS() - START; " ms"
    START = MILLIS()
    DIV()
    PRINT MILLIS() - START; " ms"
    
    START = MILLIS()
    MULI()
    PRINT MILLIS() - START; " ms"
    START = MILLIS()
    DIVI()
    PRINT MILLIS() - START; " ms"
    
    START = MILLIS()
    MULV()
    PRINT MILLIS() - START; " ms"
    START = MILLIS()
    DIVV()
    PRINT MILLIS() - START; " ms"
    
    START = MILLIS()
    MULL()
    PRINT MILLIS() - START; " ms"
    START = MILLIS()
    DIVL()
    PRINT MILLIS() - START; " ms"
END

RUN
!DASM
!NEW
!MEM
