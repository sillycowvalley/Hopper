CLS
NEW
WORD TOTAL

FUNC WLOOP()
    PRINT TOTAL
    WORD W = 10
    WHILE W > 0
        W = W - 1
        TOTAL = TOTAL + 1
        PRINT TOTAL
    WEND
    IF TOTAL <> 40 THEN
        PRINT "WHILE Failed"
    ENDIF
    PRINT TOTAL
ENDFUNC



FUNC FLOOP()
    PRINT TOTAL
    FOR I = 1 TO 10
        TOTAL = TOTAL + 1
        PRINT TOTAL
    NEXT I
    FOR I = 10 TO 1 STEP -1
        TOTAL = TOTAL + 1
        PRINT TOTAL
    NEXT I
    IF TOTAL <> 20 THEN
        PRINT "FOR Failed"
    ENDIF
    PRINT TOTAL
ENDFUNC

FUNC ULOOP()
    PRINT TOTAL
    WORD U = 10
    DO
        U = U - 1
        TOTAL = TOTAL + 1
        PRINT TOTAL
    UNTIL U = 0
    IF TOTAL <> 30 THEN
        PRINT "DO Failed"
    ENDIF
    PRINT TOTAL
ENDFUNC

BEGIN
    PRINT TOTAL
    PRINT "FLOOP:"
    FLOOP()
    PRINT TOTAL
    
    PRINT "ULOOP:"
    ULOOP()
    PRINT TOTAL
    
    PRINT "WLOOP:"
    WLOOP()
    PRINT TOTAL
    
    IF TOTAL = 40 THEN
        PRINT "Loops Passed"
    ENDIF
    IF TOTAL <> 40 THEN
        PRINT "Loops Failed"
    ENDIF
END

RUN
