CLS

! FOR/NEXT Loop Test Suite for Hopper BASIC v3 - Simplified Type System
! Following progressive isolation methodology
! Tests basic loops, STEP, nested loops, and edge cases
! 
! DESIGN: All numeric iterators use LONG type via VAR declaration
! Implicit iterators go out of scope after NEXT
! Declared iterators persist after NEXT (traditional BASIC behavior)

NEW
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

! ===== TEST 7: Iterator Scope Behavior =====
NEW
FUNC TestIteratorScope()
    VAR j
    PRINT "Testing iterator scope:"
    
    ! Test implicit iterator (goes out of scope)
    FOR i = 1 TO 3
        ! i is implicit - available within loop
    NEXT i
    PRINT "Implicit iterator 'i' correctly out of scope"
    
    ! Test declared iterator (persists)
    FOR j = 10 TO 12
        ! j is declared - available within loop
    NEXT j
    PRINT "Declared iterator j after loop: "; j; " ! expect 13"
ENDFUNC
BEGIN
    TestIteratorScope()
END
RUN

! ===== TEST 8: Global Iterator Value After Loop =====
NEW
VAR g
BEGIN
    FOR g = 1 TO 3
        ! Loop runs
    NEXT g
    PRINT "Global after FOR 1 TO 3: "; g; " ! expect 4"
END
RUN
VARS

! ===== TEST 9: Nested Loops =====
NEW
FUNC TestNested()
    VAR j
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
        VAR condition = i = 2
        IF condition THEN 
            i = i + 1 
        ENDIF
    NEXT i
    PRINT " ! expect 1 2 4 5 (skip 3)"
ENDFUNC
BEGIN
    TestModifyIter()
END
RUN

! ===== TEST 11: Large LONG Iterator Values =====
NEW
FUNC TestLongRange()
    VAR bigStart = 1000000
    VAR bigEnd = 1000003
    PRINT "LONG range 1000000 TO 1000003:"
    FOR i = bigStart TO bigEnd
        PRINT i;
    NEXT i
    PRINT " ! expect 1000000-1000003"
ENDFUNC
BEGIN
    TestLongRange()
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

! ===== TEST 14: Array Index Iteration =====
NEW
INT numbers[5]
FUNC TestArrayIter()
    VAR value = 100
    PRINT "Array index iteration:"
    FOR i = 0 TO 4
        numbers[i] = value + i
        PRINT numbers[i];
    NEXT i
    PRINT " ! expect 100 101 102 103 104"
ENDFUNC
BEGIN
    TestArrayIter()
END
RUN

! ===== TEST 15: Nested with Mixed Scopes =====
NEW
VAR outer
FUNC TestMixedScope()
    VAR inner
    PRINT "Mixed scope nested:"
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
VARS

! ===== TEST 16: Empty Loop with Declared Iterator =====
NEW
FUNC TestEmptyLoopDeclared()
    VAR i
    PRINT "Empty loop with declared iterator:"
    FOR i = 1 TO 3
    NEXT i
    PRINT "i after empty loop: "; i; " ! expect 4"
ENDFUNC
BEGIN
    TestEmptyLoopDeclared()
END
RUN

! ===== TEST 17: FOR with VAR Arguments =====
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
    TestForWithArg(10, 8)
END
RUN

! ===== TEST 18: Nested Implicit Iterators =====
NEW
FUNC TestNestedImplicit()
    PRINT "Nested implicit iterators:"
    FOR i = 1 TO 2
        FOR j = 1 TO 2
            FOR k = 1 TO 2
                PRINT i; j; k; " ";
            NEXT k
        NEXT j
        PRINT "|";
    NEXT i
    PRINT " ! expect 111 112 121 122 |211 212 221 222 |"
    PRINT "All iterators (i,j,k) now out of scope"
ENDFUNC
BEGIN
    TestNestedImplicit()
END
RUN

! ===== TEST 19: Mixed Implicit and Declared =====
NEW
VAR outer
FUNC TestMixedIterators()
    PRINT "Mixed iterator types:"
    FOR outer = 1 TO 2
        FOR inner = 1 TO 2
            PRINT outer; inner; " ";
        NEXT inner
    NEXT outer
    PRINT "| outer after: "; outer; " ! expect 3"
    PRINT "inner is out of scope (implicit)"
ENDFUNC
BEGIN
    TestMixedIterators()
END
RUN

! ===== TEST 20: Sequential Implicit Reuse =====
NEW
FUNC TestSequentialImplicit()
    PRINT "Sequential implicit iterators:"
    FOR i = 1 TO 3
        PRINT i;
    NEXT i
    PRINT " first loop done"
    FOR i = 10 TO 12
        PRINT i;
    NEXT i
    PRINT " second loop done"
    PRINT "Both 'i' variables out of scope"
ENDFUNC
BEGIN
    TestSequentialImplicit()
END
RUN

! ===== STEP Zero Test (May hang!) =====
! Uncomment with caution - might require ^C to break
!NEW
!FUNC TestStepZero()
!    PRINT "Testing STEP 0 (may hang):"
!    FOR i = 1 TO 5 STEP 0
!        PRINT i
!        VAR escape = i > 2
!        IF escape THEN
!            PRINT "Infinite detected!"
!            RETURN
!        ENDIF
!    NEXT i
!ENDFUNC
!BEGIN
!    TestStepZero()
!END
!RUN

! Memory leak detection
NEW
MEM
PRINT "FOR/NEXT tests complete - check memory vs baseline"
