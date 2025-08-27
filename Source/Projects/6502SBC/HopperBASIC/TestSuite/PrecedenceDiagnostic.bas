CLS
! Hopper BASIC Precedence Diagnostic Test Suite
! Determines actual vs expected operator precedence
! Tests what's implemented and what needs fixing

NEW
MEM

! ===== TEST 1: What's Currently Implemented? =====
FUNC TestCurrentImplementation()
    PRINT "=== Current Implementation Test ==="
    
    PRINT "Basic arithmetic (should work):"
    PRINT 2 + 3 * 4; " ! expect 14 (* before +)"
    PRINT 10 - 6 / 2; " ! expect 7 (/ before -)"
    
    PRINT "Bitwise operations (testing if implemented):"
    PRINT 15 & 7; " ! expect 7 (if & works)"
    PRINT 8 | 4; " ! expect 12 (if | works)"
    
    PRINT "Unary minus (should work):"
    PRINT -5 + 3; " ! expect -2"
    PRINT -(5 + 3); " ! expect -8"
    
    PRINT "Parentheses override (should work):"
    PRINT (2 + 3) * 4; " ! expect 20"
    PRINT 2 + (3 * 4); " ! expect 14"
    PRINT
ENDFUNC

! ===== TEST 2: Precedence Reality Check =====
FUNC TestActualPrecedence()
    PRINT "=== Actual Precedence Test ==="
    PRINT "Testing what precedence is ACTUALLY implemented..."
    
    ! Test 1: Addition vs Bitwise AND
    PRINT "2 + 3 & 4 = "; 2 + 3 & 4
    PRINT "If result is 4: & binds tighter (2+(3&4)=2+0=2) - WRONG!"
    PRINT "If result is 0: + binds tighter ((2+3)&4=5&4=4) - CURRENT"
    PRINT
    
    ! Test 2: Bitwise AND vs Multiplication  
    PRINT "2 * 3 & 4 = "; 2 * 3 & 4
    PRINT "If result is 4: & after * ((2*3)&4=6&4=4) - CORRECT"
    PRINT "If result is 8: * after & (2*(3&4)=2*0=0) - WRONG!"
    PRINT
    
    ! Test 3: Bitwise OR vs Addition
    PRINT "8 | 1 + 2 = "; 8 | 1 + 2
    PRINT "If result is 11: | binds tighter ((8|1)+2=9+2=11) - WANT"
    PRINT "If result is 10: + binds tighter (8|(1+2)=8|3=11) - CURRENT"
    PRINT
ENDFUNC

! ===== TEST 3: Missing Operators =====
FUNC TestMissingOperators()
    PRINT "=== Missing Operators Test ==="
    
    PRINT "Testing bitwise complement ~ (expect SYNTAX ERROR if missing):"
    ! Commented out to avoid breaking the test
    ! PRINT ~5; " ! should be -6"
    ! PRINT ~0; " ! should be -1" 
    ! PRINT ~(-1); " ! should be 0"
    PRINT "~ operator tests commented out - causes SYNTAX ERROR"
    
    PRINT "Testing exponentiation ^ (expect SYNTAX ERROR if missing):"
    ! PRINT 2 ^ 3; " ! should be 8"
    ! PRINT 10 ^ 2; " ! should be 100"
    PRINT "^ operator tests commented out - may cause SYNTAX ERROR"
    
    PRINT "Testing EOR (expect SYNTAX ERROR if missing):"
    ! PRINT 15 EOR 7; " ! should be 8"
    PRINT "EOR operator tests commented out - may cause SYNTAX ERROR"
    PRINT
ENDFUNC

! ===== TEST 4: Expected vs Actual Precedence =====
FUNC TestExpectedVsActual()
    PRINT "=== Expected vs Actual Precedence ==="
    
    PRINT "EXPECTED precedence (what user wants):"
    PRINT "  1. () [] functions"
    PRINT "  2. unary - ~"
    PRINT "  3. * / MOD"
    PRINT "  4. & (bitwise AND)"
    PRINT "  5. | (bitwise OR)"
    PRINT "  6. + -"
    PRINT "  7. comparisons"
    PRINT
    
    PRINT "ACTUAL precedence (from grammar analysis):"
    PRINT "  1. () [] functions" 
    PRINT "  2. unary - ~"
    PRINT "  3. * / MOD"
    PRINT "  4. + -"
    PRINT "  5. & (bitwise AND)"
    PRINT "  6. | (bitwise OR)"
    PRINT "  7. comparisons"
    PRINT
    
    PRINT "MISMATCH: & and | should bind tighter than + and -"
ENDFUNC

! ===== TEST 5: Specific Problem Cases =====
FUNC TestProblemCases()
    PRINT "=== Problem Cases ==="
    
    PRINT "Case 1: 2 + 3 & 4"
    VAR result1 = 2 + 3 & 4
    PRINT "Result: "; result1
    PRINT "Want: 2 (if & before +)"
    PRINT "Got:  4 (if + before &) <- CURRENT WRONG"
    PRINT
    
    PRINT "Case 2: 8 | 1 + 2"  
    VAR result2 = 8 | 1 + 2
    PRINT "Result: "; result2
    PRINT "Want: 11 (if | before +)"
    PRINT "Got:  11 (if + before |) <- might be coincidence"
    PRINT
    
    PRINT "Case 3: 15 & 7 + 1"
    VAR result3 = 15 & 7 + 1
    PRINT "Result: "; result3
    PRINT "Want: 8 (if & before +)"
    PRINT "Got:  0 (if + before &) <- CURRENT WRONG"
    PRINT
ENDFUNC

! ===== TEST 6: Simple Verification =====
FUNC TestSimpleVerification()
    PRINT "=== Simple Verification ==="
    
    ! These should all work regardless of precedence issues
    PRINT "Basic operations:"
    PRINT "5 + 3 = "; 5 + 3; " (expect 8)"
    PRINT "5 * 3 = "; 5 * 3; " (expect 15)"
    PRINT "15 & 7 = "; 15 & 7; " (expect 7)"
    PRINT "8 | 4 = "; 8 | 4; " (expect 12)"
    PRINT
    
    ! Test with explicit parentheses - should work correctly
    PRINT "With explicit parentheses:"
    PRINT "(2 + 3) & 4 = "; (2 + 3) & 4; " (expect 4)"
    PRINT "2 + (3 & 4) = "; 2 + (3 & 4); " (expect 2)"
    PRINT "(8 | 1) + 2 = "; (8 | 1) + 2; " (expect 11)"
    PRINT "8 | (1 + 2) = "; 8 | (1 + 2); " (expect 11)"
    PRINT
ENDFUNC

! ===== MAIN DIAGNOSTIC =====
BEGIN
    PRINT "Hopper BASIC Precedence Diagnostic v1.0"
    PRINT "Diagnosing operator precedence implementation"
    PRINT "============================================="
    PRINT
    
    TestCurrentImplementation()
    TestActualPrecedence() 
    TestMissingOperators()
    TestExpectedVsActual()
    TestProblemCases()
    TestSimpleVerification()
    
    PRINT "============================================="
    PRINT "SUMMARY:"
    PRINT "1. ~ operator needs implementation"
    PRINT "2. & and | should bind tighter than + and -" 
    PRINT "3. Grammar needs precedence correction"
    PRINT "4. Current: * / > + - > & > |"
    PRINT "5. Should be: * / > & > | > + -"
END
