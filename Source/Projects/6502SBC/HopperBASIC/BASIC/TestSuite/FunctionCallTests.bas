CLS
! =================================================================
! FUNCTION CALL ARGUMENT VALIDATION TEST SUITE
! =================================================================
! Tests function call error handling in both compiled and REPL contexts
! Each test isolated to prevent early termination on first error
! =================================================================

NEW

! Test functions
FUNC TestFunc(a, b)
    PRINT "a="; a; " b="; b
ENDFUNC

FUNC NoParams()
    PRINT "No parameters function called"
ENDFUNC

! =================================================================
! COMPILED CONTEXT TESTS (BEGIN/END blocks)
! =================================================================

! Test 1: Correct calls (baseline)
BEGIN
    PRINT "=== COMPILED CONTEXT TESTS ==="
    PRINT "1. Correct calls:"
    TestFunc(10, 20)
    NoParams()
END
RUN

! Test 2: Bare identifier in compiled context
BEGIN
    PRINT "2. Bare identifier (compiled):"
    TestFunc
    PRINT "Should not reach this line"
END
RUN

! Test 3: Wrong argument counts in compiled context
BEGIN
    PRINT "3. Wrong arg counts (compiled):"
    TestFunc()
    PRINT "Should not reach this line"
END
RUN

BEGIN
    PRINT "4. Too few args (compiled):"
    TestFunc(10)
    PRINT "Should not reach this line"
END
RUN

BEGIN
    PRINT "5. Too many args (compiled):"
    TestFunc(1, 2, 3, 4)
    PRINT "Extra args behavior test"
END
RUN

! =================================================================
! REPL CONTEXT TESTS (Direct commands)
! =================================================================

NEW
PRINT "==================================================================="
PRINT "NOW TEST REPL CONTEXT - Type these commands manually:"
PRINT "==================================================================="
PRINT

! Reload functions for REPL testing
FUNC TestFunc(a, b)
    PRINT "a="; a; " b="; b
ENDFUNC

FUNC NoParams()
    PRINT "No parameters function called"
ENDFUNC

! REPL Test 1: Correct calls
PRINT "REPL Test 1 - Correct calls:"
TestFunc(10, 20)
NoParams()
PRINT

! REPL Test 2: Bare identifiers
PRINT "REPL Test 2 - Bare identifiers:"
PRINT "About to call TestFunc without ()..."
TestFunc
PRINT "TestFunc returned OK (REPL behavior)"
PRINT

! REPL Test 3: Wrong argument counts
PRINT "REPL Test 3 - Empty parentheses:"
TestFunc()
PRINT "If you see this, TestFunc() executed despite error"
PRINT

PRINT "REPL Test 4 - Too few arguments:"
TestFunc(10)
PRINT "If you see this, TestFunc(10) executed despite error"
PRINT

PRINT "REPL Test 5 - Too many arguments:"
TestFunc(1, 2, 3, 4)
PRINT "Extra arguments ignored"
PRINT

PRINT "REPL Test 6 - Zero-param with args:"
NoParams(99)
PRINT "Extra argument ignored"
PRINT

! Summary
NEW
PRINT "==================================================================="
PRINT "FUNCTION CALL VALIDATION SUMMARY:"
PRINT "==================================================================="
PRINT "COMPILED vs REPL BEHAVIOR DIFFERENCES:"
PRINT "- Bare identifier: UNDEFINED IDENTIFIER vs OK"
PRINT "- Wrong arg count: Same INTERNAL ERROR in both"
PRINT "- Extra args: Silently ignored in both contexts"
PRINT
PRINT "ISSUES TO FIX:"
PRINT "1. Bare identifiers should error in REPL context"
PRINT "2. Replace INTERNAL ERROR with ARG COUNT ERROR"  
PRINT "3. Validate argument count and reject extras"
PRINT "==================================================================="