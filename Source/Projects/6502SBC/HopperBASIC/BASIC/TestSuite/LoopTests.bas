CLS
! Loop & IF Smoke Tests for Hopper BASIC v3
! Tests FOR, WHILE, DO-UNTIL loops plus IF statements
! Uses cumulative counter to verify execution flow

NEW
MEM  ! Baseline memory check

VAR TOTAL

! FOR Loop Test: Forward and backward iteration
! Should add 20 total (10 forward + 10 backward)
FUNC FLOOP()
    VAR expected = 20
    FOR I = 1 TO 10
        TOTAL = TOTAL + 1
    NEXT I
    FOR I = 10 TO 1 STEP -1
        TOTAL = TOTAL + 1
    NEXT I
    IF TOTAL <> expected THEN
        PRINT "FOR Failed - got "; TOTAL; " expected "; expected
    ENDIF
    PRINT "FOR total: "; TOTAL
ENDFUNC

! DO-UNTIL Loop Test: Post-condition loop
! Should add 10 more (TOTAL becomes 30)
FUNC ULOOP()
    VAR U = 10
    VAR expected = 30
    DO
        U = U - 1
        TOTAL = TOTAL + 1
    UNTIL U = 0
    IF TOTAL <> expected THEN
        PRINT "DO-UNTIL Failed - got "; TOTAL; " expected "; expected
    ENDIF
    PRINT "DO-UNTIL total: "; TOTAL
ENDFUNC

! WHILE Loop Test: Pre-condition loop  
! Should add 10 more (TOTAL becomes 40)
FUNC WLOOP()
    VAR W = 10
    VAR expected = 40
    WHILE W > 0
        W = W - 1
        TOTAL = TOTAL + 1
    WEND
    IF TOTAL <> expected THEN
        PRINT "WHILE Failed - got "; TOTAL; " expected "; expected
    ENDIF
    PRINT "WHILE total: "; TOTAL
ENDFUNC

! IF Statement Test: Condition evaluation and branching
FUNC IFTEST()
    VAR score = 85
    VAR grade = ""
    VAR bonus = FALSE
    VAR passWithBonus
    VAR expectedGrade
    
    IF score >= 80 THEN
        grade = "PASS"
        IF score >= 90 THEN
            bonus = TRUE
            grade = "EXCELLENT"
        ELSE
            grade = "GOOD"
        ENDIF
    ELSE
        grade = "FAIL"
    ENDIF
    
    passWithBonus = score >= 80 AND bonus = FALSE
    IF passWithBonus THEN
        PRINT "Grade: "; grade; " (good pass)"
    ENDIF
    
    expectedGrade = "GOOD"
    IF grade = expectedGrade THEN
        PRINT "IF test passed"
    ELSE
        PRINT "IF test failed - got "; grade; " expected "; expectedGrade
    ENDIF
ENDFUNC

BEGIN
    TOTAL = 0
    PRINT "Loop smoke tests starting..."
    
    VAR finalExpected = 40
    
    FLOOP()
    ULOOP() 
    WLOOP()
    IFTEST()
    
    
    IF TOTAL = finalExpected THEN
        PRINT "*** ALL TESTS PASSED ***"
    ELSE
        PRINT "*** TESTS FAILED *** Expected:"; finalExpected; " Got:"; TOTAL
    ENDIF
END

RUN

! Final memory check - compare with baseline
NEW
MEM  
