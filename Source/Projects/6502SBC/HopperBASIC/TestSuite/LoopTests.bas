CLS
NEW
WORD TOTAL

FUNC WLOOP()
    WORD W = 10
    WHILE W > 0
        W = W - 1
        TOTAL = TOTAL + 1
    WEND
    IF TOTAL <> 40 THEN
        PRINT "WHILE Failed"
    ENDIF
    PRINT TOTAL
ENDFUNC



FUNC FLOOP()
    FOR I = 1 TO 10
        TOTAL = TOTAL + 1
    NEXT I
    FOR I = 10 TO 1 STEP -1
        TOTAL = TOTAL + 1
    NEXT I
    IF TOTAL <> 20 THEN
        PRINT "FOR Failed"
    ENDIF
    PRINT TOTAL
ENDFUNC

FUNC ULOOP()
    WORD U = 10
    DO
        U = U - 1
        TOTAL = TOTAL + 1
    UNTIL U = 0
    IF TOTAL <> 30 THEN
        PRINT "DO Failed"
    ENDIF
    PRINT TOTAL
ENDFUNC

BEGIN
    PRINT "FLOOP:"
    FLOOP()
    
    PRINT "ULOOP:"
    ULOOP()
    
    PRINT "WLOOP:"
    WLOOP()
    
    IF TOTAL = 40 THEN
        PRINT "Loops Passed"
    ENDIF
    IF TOTAL <> 40 THEN
        PRINT "Loops Failed"
    ENDIF
END

RUN
