CLS
NEW
FUNC TestFailures()
    PRINT "=== FAILURES ==="
    
    ! PRINT "is ";~0; " expect -1"
    
    PRINT "is "; 2 + 3 & 4; ", expect 2"
    PRINT "is "; 2 + (3 & 4); ", expect 2 (explicit)"
    
    PRINT "is "; 12 & 4 + 3; ", expect 7" 
    PRINT "is "; (12 & 4) + 3; ", expect 7 (explicit)"
    
    PRINT "is "; 10 - 6 & 8; ", expect 10"
    PRINT "is "; 10 - (6 & 8); ", expect 10 (explicit)"
    
    PRINT "is "; 8 - 2 | 4; ", expect 6"
    PRINT "is "; (8 - 2) | 4; ", expect 6 (explicit)"
    
    ! PRINT "is "; ~5 + 1; ", expect -5"
ENDFUNC

BEGIN
    TestFailures()
END
RUN
