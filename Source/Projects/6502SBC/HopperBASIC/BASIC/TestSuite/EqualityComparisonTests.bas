! Comparison Tests for Hopper BASIC v3 - Simplified Type System
! Testing = and <> operators with LONG as primary numeric type
! Following progressive isolation methodology
! All tests included - failures expected and documented

! ===== TEST 1: BIT Comparisons (Isolated) =====
NEW
FUNC TestBitCompare()
    VAR b1 = TRUE
    VAR b2 = FALSE
    VAR b3 = TRUE
    PRINT "BIT Tests:"
    PRINT b1 = b3; " ! expect TRUE"
    PRINT b1 = b2; " ! expect FALSE"
    PRINT b1 <> b2; " ! expect TRUE"
    PRINT b2 <> b2; " ! expect FALSE"
ENDFUNC
BEGIN
    TestBitCompare()
END
RUN

! ===== TEST 2: LONG Comparisons (Primary Numeric Type) =====
NEW
FUNC TestLongCompare()
    VAR num1 = 0
    VAR num2 = 2147483647
    VAR num3 = -2147483648
    VAR num4 = 1000000
    VAR num5 = 1000000
    PRINT "LONG Tests:"
    PRINT num1 = 0; " ! expect TRUE"
    PRINT num2 = 2147483647; " ! expect TRUE"
    PRINT num3 = -2147483648; " ! expect TRUE"
    PRINT num4 = num5; " ! expect TRUE"
    PRINT num1 <> num2; " ! expect TRUE"
    PRINT num3 <> num4; " ! expect TRUE"
ENDFUNC
BEGIN
    TestLongCompare()
END
RUN

! ===== TEST 3: CHAR Comparisons (Isolated) =====
NEW
FUNC TestCharCompare()
    VAR c1 = 'A'
    VAR c2 = 'Z'
    VAR c3 = 'A'
    VAR c4 = '0'
    PRINT "CHAR Tests:"
    PRINT c1 = c3; " ! expect TRUE"
    PRINT c1 = 'A'; " ! expect TRUE"
    PRINT c1 <> c2; " ! expect TRUE"
    PRINT c4 = '0'; " ! expect TRUE"
    PRINT c2 <> c4; " ! expect TRUE"
ENDFUNC
BEGIN
    TestCharCompare()
END
RUN

! ===== TEST 4: STRING Comparisons (Isolated) =====
NEW
FUNC TestStringCompare()
    VAR s1 = "HELLO"
    VAR s2 = "WORLD"
    VAR s3 = "HELLO"
    VAR s4 = ""
    PRINT "STRING Tests:"
    PRINT s1 = s3; " ! expect TRUE"
    PRINT s1 = "HELLO"; " ! expect TRUE"
    PRINT s1 <> s2; " ! expect TRUE"
    PRINT s4 = ""; " ! expect TRUE"
    PRINT s4 <> "X"; " ! expect TRUE"
ENDFUNC
BEGIN
    TestStringCompare()
END
RUN

! ===== TEST 5: VAR Type Evolution Comparisons =====
NEW
FUNC TestVarEvolution()
    VAR v1 = 42
    VAR v2 = 42
    PRINT "VAR Evolution Tests:"
    PRINT v1 = v2; " ! expect TRUE"
    PRINT v1 = 42; " ! expect TRUE"
    
    ! Change types and test
    v1 = "HELLO"
    v2 = "HELLO"
    PRINT v1 = v2; " ! expect TRUE"
    PRINT v1 = "HELLO"; " ! expect TRUE"
    
    ! Mix types
    v1 = TRUE
    v2 = TRUE  
    PRINT v1 = v2; " ! expect TRUE"
    PRINT v1 = TRUE; " ! expect TRUE"
ENDFUNC
BEGIN
    TestVarEvolution()
END
RUN

! ===== TEST 6: Explicit Array Type Comparisons =====
! Arrays still use explicit types for memory efficiency
NEW
BIT flags[3]
CHAR letters[3]
INT numbers[3]

FUNC TestArrayCompare()
    PRINT "Array Element Tests:"
    flags[0] = TRUE
    flags[1] = FALSE
    PRINT flags[0] = TRUE; " ! expect TRUE"
    PRINT flags[0] <> flags[1]; " ! expect TRUE"
    
    letters[0] = 'A'
    letters[1] = 'A'
    PRINT letters[0] = letters[1]; " ! expect TRUE"
    PRINT letters[0] = 'A'; " ! expect TRUE"
    
    numbers[0] = 100
    numbers[1] = 100
    PRINT numbers[0] = numbers[1]; " ! expect TRUE"
    PRINT numbers[0] = 100; " ! expect TRUE"
ENDFUNC
BEGIN
    TestArrayCompare()
END
RUN

! ===== TEST 7: Large LONG Values =====
NEW
FUNC TestLargeLong()
    VAR big1 = 1000000
    VAR big2 = 2000000
    VAR big3 = 1000000
    PRINT "Large LONG Tests:"
    PRINT big1 = big3; " ! expect TRUE"
    PRINT big1 <> big2; " ! expect TRUE"
    PRINT big1 = 1000000; " ! expect TRUE"
    PRINT big2 = 2000000; " ! expect TRUE"
ENDFUNC
BEGIN
    TestLargeLong()
END
RUN

! ===== TEST 8: Mixed Literal Comparisons =====
NEW
FUNC TestMixedLiterals()
    VAR numVar = 42
    VAR charVar = 'X'
    VAR bitVar = TRUE
    VAR stringVar = "TEST"
    PRINT "Mixed Literal Tests:"
    PRINT numVar = 42; " ! expect TRUE"
    PRINT charVar = 'X'; " ! expect TRUE"
    PRINT bitVar = TRUE; " ! expect TRUE"
    PRINT stringVar = "TEST"; " ! expect TRUE"
    PRINT numVar <> 999; " ! expect TRUE"
    PRINT charVar <> 'Y'; " ! expect TRUE"
    PRINT bitVar <> FALSE; " ! expect TRUE"
    PRINT stringVar <> "OTHER"; " ! expect TRUE"
ENDFUNC
BEGIN
    TestMixedLiterals()
END
RUN

! ===== TEST 9: Type Mismatch Errors =====
! Test these individually to catch expected errors
NEW
FUNC TestTypeMismatch1()
    VAR numVar = 42
    VAR bitVar = TRUE
    PRINT "LONG = BIT: "; numVar = bitVar
ENDFUNC
BEGIN
    PRINT "Testing LONG = BIT (expect TYPE MISMATCH):"
    TestTypeMismatch1()
END
RUN

NEW
FUNC TestTypeMismatch2()
    VAR stringVar = "HELLO"
    VAR numVar = 5
    PRINT "STRING = LONG: "; stringVar = numVar
ENDFUNC
BEGIN
    PRINT "Testing STRING = LONG (expect TYPE MISMATCH):"
    TestTypeMismatch2()
END
RUN

NEW
FUNC TestTypeMismatch3()
    VAR charVar = 'A'
    VAR numVar = 65
    PRINT "CHAR = LONG: "; charVar = numVar
ENDFUNC
BEGIN
    PRINT "Testing CHAR = LONG (expect TYPE MISMATCH):"
    TestTypeMismatch3()
END
RUN

NEW
FUNC TestTypeMismatch4()
    VAR charVar = 'A'
    VAR stringVar = "A"
    PRINT "CHAR = STRING: "; charVar = stringVar
ENDFUNC
BEGIN
    PRINT "Testing CHAR = STRING (expect TYPE MISMATCH):"
    TestTypeMismatch4()
END
RUN

NEW
FUNC TestTypeMismatch5()
    VAR bitVar = TRUE
    VAR stringVar = "TRUE"
    PRINT "BIT = STRING: "; bitVar = stringVar
ENDFUNC
BEGIN
    PRINT "Testing BIT = STRING (expect TYPE MISMATCH):"
    TestTypeMismatch5()
END
RUN

! ===== TEST 10: Zero and Negative LONG Edge Cases =====
NEW
FUNC TestLongEdges()
    VAR zero = 0
    VAR neg = -1
    VAR posMax = 2147483647
    VAR negMax = -2147483648
    PRINT "LONG Edge Cases:"
    PRINT zero = 0; " ! expect TRUE"
    PRINT neg = -1; " ! expect TRUE"
    PRINT posMax = 2147483647; " ! expect TRUE"
    PRINT negMax = -2147483648; " ! expect TRUE"
    PRINT zero <> neg; " ! expect TRUE"
    PRINT posMax <> negMax; " ! expect TRUE"
ENDFUNC
BEGIN
    TestLongEdges()
END
RUN

! ===== Memory Check =====
NEW
MEM
PRINT "Comparison tests complete - simplified type system"
