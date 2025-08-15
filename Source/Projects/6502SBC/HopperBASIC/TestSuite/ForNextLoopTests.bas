CLS

! FOR/NEXT Loop Test Suite for Hopper BASIC
! Following progressive isolation methodology
! Tests basic loops, STEP, nested loops, type promotion, optimization

MEM 

! ===== TEST 1: Basic FOR/NEXT (Isolated) =====
NEW
FUNC TestBasicFor()
    PRINT "Basic FOR 1 TO 5:"
    FOR i = 1 TO 5
        PRINT i;
    NEXT i
    PRINT " ! expect 1 2 3 4 5"
ENDFUNC
BEGIN
    TestBasicFor()
END
RUN

! ===== TEST 2: FOR with Explicit STEP 1 =====
NEW
FUNC TestStep1()
    PRINT "FOR 1 TO 5 STEP 1:"
    FOR i = 1 TO 5 STEP 1
        PRINT i;
    NEXT i
    PRINT " ! expect 1 2 3 4 5"
ENDFUNC
BEGIN
    TestStep1()
END
RUN

! ===== TEST 3: FOR with Negative STEP =====
NEW
FUNC TestNegativeStep()
    PRINT "FOR 10 TO 1 STEP -2:"
    FOR i = 10 TO 1 STEP -2
        PRINT i;
    NEXT i
    PRINT " ! expect 10 8 6 4 2"
ENDFUNC
BEGIN
    TestNegativeStep()
END
RUN

! ===== TEST 4: FOR with Large STEP =====
NEW
FUNC TestLargeStep()
    PRINT "FOR 0 TO 10 STEP 3:"
    FOR i = 0 TO 10 STEP 3
        PRINT i;
    NEXT i
    PRINT " ! expect 0 3 6 9"
ENDFUNC
BEGIN
    TestLargeStep()
END
RUN

! ===== TEST 5: FOR Start > End (Positive STEP) =====
NEW
FUNC TestNoIteration()
    PRINT "FOR 10 TO 1 STEP 1:"
    FOR i = 10 TO 1 STEP 1
        PRINT i; " OOPS!";
    NEXT i
    PRINT "Done ! expect no output"
ENDFUNC
BEGIN
    TestNoIteration()
END
RUN

! ===== TEST 6: Single Iteration =====
NEW
FUNC TestSingleIter()
    PRINT "FOR 5 TO 5:"
    FOR i = 5 TO 5
        PRINT i;
    NEXT i
    PRINT " ! expect 5"
ENDFUNC
BEGIN
    TestSingleIter()
END
RUN

! ===== TEST 7: Iterator Value After Loop (Local) =====
NEW
FUNC TestIterAfter()
    INT j
    FOR i = 1 TO 3
        ! Loop body
    NEXT i
    PRINT "After FOR 1 TO 3: "; i; " ! expect 4"
    FOR j = 10 TO 6 STEP -2
        ! Loop body
    NEXT j
    PRINT "After FOR 10 TO 6 STEP -2: "; j; " ! expect 4"
ENDFUNC
BEGIN
    TestIterAfter()
END
RUN

! ===== TEST 8: Global Iterator Value After Loop =====
NEW
INT g
BEGIN
    FOR g = 1 TO 3
        ! Loop runs
    NEXT g
    PRINT "Global after FOR 1 TO 3: "; g; " ! expect 4"
END
RUN
  ! Check global value persists
VARS

! ===== TEST 9: Nested Loops (Declaration Required) =====
NEW
FUNC TestNested()
    INT j  ! Must declare before first flow control
    PRINT "Nested 3x3:"
    FOR i = 1 TO 3
        FOR j = 1 TO 3
            PRINT i; ","; j; " ";
        NEXT j
        PRINT
    NEXT i
    PRINT "Done"
ENDFUNC
BEGIN
    TestNested()
END
RUN

! ===== TEST 10: Modify Iterator Inside Loop =====
NEW
FUNC TestModifyIter()
    PRINT "Modify iterator (i=i+1 when i=2):"
    FOR i = 1 TO 5
        PRINT i;
        IF i = 2 THEN i = i + 1 ENDIF
    NEXT i
    PRINT " ! expect 1 2 4 5 (skip 3)"
ENDFUNC
BEGIN
    TestModifyIter()
END
RUN

! ===== TEST 11: BYTE Iterator with INT Bounds =====
NEW
FUNC TestByteIter()
    BYTE b
    PRINT "BYTE iter, INT bounds:"
    FOR b = 1 TO 5
        PRINT b;
    NEXT b
    PRINT " ! expect 1 2 3 4 5"
    PRINT "After: b="; b; " ! expect 6"
ENDFUNC
BEGIN
    TestByteIter()
END
RUN

! ===== TEST 12: FORITF Optimization Test =====
NEW
FUNC TestForitf()
    ! This should trigger FORITF optimization:
    ! FROM=1 (positive), TO=10 (positive), FROM<TO, STEP=1 (default)
    PRINT "FORITF candidate 1 TO 10:"
    FOR i = 1 TO 10
        PRINT i;
    NEXT i
    PRINT " ! expect 1-10"
ENDFUNC
BEGIN
    TestForitf()
END
RUN

! ===== TEST 13: Non-FORITF (Negative Start) =====
NEW
FUNC TestNoForitf()
    ! Should NOT trigger FORITF: FROM is negative
    PRINT "Non-FORITF -2 TO 2:"
    FOR i = -2 TO 2
        PRINT i;
    NEXT i
    PRINT " ! expect -2 -1 0 1 2"
ENDFUNC
BEGIN
    TestNoForitf()
END
RUN

! ===== TEST 14: Type Promotion Test =====
NEW
FUNC TestTypePromo()
    BYTE b = 2
    INT i = 5
    WORD w = 8
    PRINT "Type promotion in FOR:"
    FOR x = b TO w
        PRINT x;
    NEXT x
    PRINT " ! expect 2 3 4 5 6 7 8"
ENDFUNC
BEGIN
    TestTypePromo()
END
RUN

! ===== TEST 15: Nested with Mixed Scopes =====
NEW
INT outer
FUNC TestMixedScope()
    INT inner
    FOR outer = 1 TO 2
        FOR inner = 1 TO 2
            PRINT outer; inner;
        NEXT inner
    NEXT outer
    PRINT " ! expect 1 1 1 2 2 1 2 2"
ENDFUNC
BEGIN
    TestMixedScope()
END
RUN
  ! Check outer
VARS

! ===== TEST 16: Empty Loop Body =====
NEW
FUNC TestEmptyLoop()
    PRINT "Empty loop executes:"
    FOR i = 1 TO 3
    NEXT i
    PRINT i; " ! expect 4 (after FOR 1 TO 3)"
ENDFUNC
BEGIN
    TestEmptyLoop()
END
RUN

! ===== TEST 17: WORD Iterator Large Range =====
!NEW
!FUNC TestWordRange()
!    WORD w
!    PRINT "WORD 65530 TO 65535:"
!    FOR w = 65530 TO 65535
!        PRINT w,
!    NEXT w
!    PRINT " ! expect 65530-65535"
!ENDFUNC
!BEGIN
!    TestWordRange()
!END
!RUN

! ===== TEST 18: FOR with Arguments =====
NEW
FUNC TestForWithArg(start, stop)
    PRINT "FOR "; start; " TO "; stop; ":"
    FOR i = start TO stop
        PRINT i;
    NEXT i
    PRINT
ENDFUNC
BEGIN
    TestForWithArg(3, 6)
    TestForWithArg(5, 5)
    TestForWithArg(10, 8)  ! No iteration
END
RUN

! ===== TEST 19: STEP Zero Test (May hang!) =====
! Uncomment with caution - might require ^C
!NEW
!FUNC TestStepZero()
!    PRINT "Testing STEP 0 (may hang):"
!    FOR i = 1 TO 5 STEP 0
!        PRINT i
!        IF i > 2 THEN PRINT "Infinite!" : RETURN ENDIF
!    NEXT i
!ENDFUNC
!BEGIN
!    TestStepZero()
!END
!RUN

! ===== TEST 20: Memory Check =====
NEW
MEM
PRINT "Memory check complete"
