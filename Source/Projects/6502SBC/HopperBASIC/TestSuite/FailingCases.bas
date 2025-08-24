CLS
! LONG Display Issue Tests - Only Failing Cases
! Display problem for LONG values < -32768

NEW

! ===== FAILING: LONG Display for Values < -32768 =====
FUNC TestLongDisplayIssues()
    LONG l1 = -40000
    LONG l2 = -35000
    LONG l3 = -75000
    PRINT "LONG Display Issues (FAILING):"
    PRINT l1; " ! expect -40000 (shows 25536)"
    PRINT l2; " ! expect -35000 (shows 30536)" 
    PRINT l3; " ! expect -75000 (shows 56072)"
ENDFUNC

BEGIN
    TestLongDisplayIssues()
END

RUN
