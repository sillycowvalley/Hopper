NEW
CLS
MEM

! Noel's RetroLab Benchmark

BEGIN
    WORD START = MILLIS()
    LONG S
    WORD J
    FOR i=1 TO 10
        s=0
        FOR j=1 TO 1000
            s=s+j
        NEXT j
        PRINT ".";
    NEXT i
    PRINT s
    PRINT MILLIS() - START; " ms"
END

RUN
DASM
!NEW
!MEM
