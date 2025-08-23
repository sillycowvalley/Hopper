CLS
NEW
BEGIN
    WORD X = 10
    IF X > 5 THEN
        PRINT "big number"
        Y = X * 2
    ELSE
        PRINT "small number"  
        Y = X + 1
    ENDIF
    IF X > 5 THEN                                      ! Multi-line
        PRINT "big number"
        Y = X * 2
    ELSE
        PRINT "small number"  
        Y = X + 1
    ENDIF
    
    IF X > 5 THEN PRINT "big" ENDIF
    IF X > 5 THEN PRINT "big" ENDIF                    ! Single-line, no ELSE
    IF X > 5 THEN PRINT "big" ELSE PRINT "small" ENDIF
    IF X > 5 THEN PRINT "big" ELSE PRINT "small" ENDIF ! Single-line with ELSE  
    
END
RUN
