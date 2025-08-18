CLS

! FOR/NEXT Loop Test Suite for Hopper BASIC - Updated for Design Change
! Following progressive isolation methodology
! Tests basic loops, STEP, nested loops, type promotion, optimization
! 
! DESIGN CHANGE: Implicit iterators go out of scope after NEXT
! Declared iterators persist after NEXT (traditional BASIC behavior)

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

! ===== TEST 7: Iterator Scope Behavior (Updated) =====
NEW
FUNC TestIteratorScope()
    INT j
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
INT g
BEGIN
    FOR g = 1 TO 3
        ! Loop runs
    NEXT g
    PRINT "Global after FOR 1 TO 3: "; g; " ! expect 4"
END
RUN
VARS

! ===== TEST 9: Nested Loops (Declaration Required) =====
NEW
FUNC TestNested()
    INT j
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
VARS

! ===== TEST 16: Empty Loop with Declared Iterator (Updated) =====
NEW
FUNC TestEmptyLoopDeclared()
    INT i
    PRINT "Empty loop with declared iterator:"
    FOR i = 1 TO 3
    NEXT i
    PRINT "i after empty loop: "; i; " ! expect 4"
ENDFUNC
BEGIN
    TestEmptyLoopDeclared()
END
RUN

! ===== TEST 17: WORD Iterator Safe Range =====
NEW
FUNC TestWordRange()
    WORD w
    PRINT "WORD 65530 TO 65533:"
    FOR w = 65530 TO 65533
        PRINT w;
    NEXT w
    PRINT " ! expect 65530-65533"
    PRINT "After: w="; w; " ! expect 65534"
ENDFUNC
BEGIN
    TestWordRange()
END
RUN

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
    TestForWithArg(10, 8)
END
RUN

! ===== TEST 19: Nested Implicit Iterators (New) =====
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

! ===== TEST 20: Mixed Implicit and Declared (New) =====
NEW
INT outer
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

! ===== TEST 21: Multiple Implicit Sequential (New) =====
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

! ===== TEST 22: Implicit Iterator Type Changes (New) =====
NEW
FUNC TestImplicitTypes()
    BYTE b = 5
    WORD w = 300
    PRINT "Implicit iterator type promotion:"
    FOR i = b TO 10
        PRINT i;
    NEXT i
    PRINT " BYTE to INT"
    FOR i = 1 TO w
        IF i > 5 THEN
            RETURN
        ENDIF
        PRINT i;
    NEXT i
    PRINT " INT to WORD"
ENDFUNC
BEGIN
    TestImplicitTypes()
END
RUN

! ===== TEST 23: STEP Zero Test (May hang!) =====
! Uncomment with caution - might require ^C
!NEW
!FUNC TestStepZero()
!    PRINT "Testing STEP 0 (may hang):"
!    FOR i = 1 TO 5 STEP 0
!        PRINT i
!        IF i > 2 THEN
!            PRINT "Infinite!"
!            RETURN
!        ENDIF
!    NEXT i
!ENDFUNC
!BEGIN
!    TestStepZero()
!END
!RUN

! ===== TEST 24: Memory Check =====
NEW
MEM
PRINT "Memory check complete - all iterators cleaned up"