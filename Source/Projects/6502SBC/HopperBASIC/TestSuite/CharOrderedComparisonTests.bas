CLS
! CHAR Ordered Comparison Test Suite for Hopper BASIC
! Tests newly implemented < > <= >= operators for CHAR type
! Following progressive isolation methodology
! Tests type mismatch protection and CHR/ASC conversion comparisons

NEW
MEM

! ===== TEST 1: Basic CHAR Ordered Comparisons (Isolated) =====
NEW
FUNC TestCharOrdered()
    CHAR c1 = 'A'
    CHAR c2 = 'B'
    CHAR c3 = 'Z'
    CHAR c4 = 'A'
    PRINT "CHAR Ordered Tests:"
    PRINT c1 < c2; " ! expect TRUE (A < B)"
    PRINT c2 < c3; " ! expect TRUE (B < Z)"
    PRINT c3 < c1; " ! expect FALSE (Z < A)"
    PRINT c1 < c4; " ! expect FALSE (A < A)"
ENDFUNC
BEGIN
    TestCharOrdered()
END
RUN

! ===== TEST 2: CHAR Greater Than (Isolated) =====
NEW
FUNC TestCharGreater()
    CHAR c1 = 'Z'
    CHAR c2 = 'A'
    CHAR c3 = 'M'
    CHAR c4 = 'M'
    PRINT "CHAR Greater Tests:"
    PRINT c1 > c2; " ! expect TRUE (Z > A)"
    PRINT c3 > c2; " ! expect TRUE (M > A)"
    PRINT c2 > c1; " ! expect FALSE (A > Z)"
    PRINT c3 > c4; " ! expect FALSE (M > M)"
ENDFUNC
BEGIN
    TestCharGreater()
END
RUN

! ===== TEST 3: CHAR Less Than or Equal (Isolated) =====
NEW
FUNC TestCharLessEqual()
    CHAR c1 = 'A'
    CHAR c2 = 'B'
    CHAR c3 = 'A'
    PRINT "CHAR <= Tests:"
    PRINT c1 <= c2; " ! expect TRUE (A <= B)"
    PRINT c1 <= c3; " ! expect TRUE (A <= A)"
    PRINT c2 <= c1; " ! expect FALSE (B <= A)"
ENDFUNC
BEGIN
    TestCharLessEqual()
END
RUN

! ===== TEST 4: CHAR Greater Than or Equal (Isolated) =====
NEW
FUNC TestCharGreaterEqual()
    CHAR c1 = 'Z'
    CHAR c2 = 'Y'
    CHAR c3 = 'Z'
    PRINT "CHAR >= Tests:"
    PRINT c1 >= c2; " ! expect TRUE (Z >= Y)"
    PRINT c1 >= c3; " ! expect TRUE (Z >= Z)"
    PRINT c2 >= c1; " ! expect FALSE (Y >= Z)"
ENDFUNC
BEGIN
    TestCharGreaterEqual()
END
RUN

! ===== TEST 5: Digit Character Comparisons =====
NEW
FUNC TestDigitChars()
    CHAR d0 = '0'
    CHAR d5 = '5'
    CHAR d9 = '9'
    PRINT "Digit CHAR Tests:"
    PRINT d0 < d5; " ! expect TRUE (0 < 5)"
    PRINT d5 < d9; " ! expect TRUE (5 < 9)"
    PRINT d9 > d0; " ! expect TRUE (9 > 0)"
    PRINT d5 <= d5; " ! expect TRUE (5 <= 5)"
    PRINT d9 >= d5; " ! expect TRUE (9 >= 5)"
ENDFUNC
BEGIN
    TestDigitChars()
END
RUN

! ===== TEST 6: Mixed Case Comparisons =====
NEW
FUNC TestMixedCase()
    CHAR upper = 'A'
    CHAR lower = 'a'
    CHAR upperZ = 'Z'
    CHAR lowerZ = 'z'
    PRINT "Mixed Case Tests:"
    PRINT upper < lower; " ! expect TRUE (A < a)"
    PRINT upperZ < lower; " ! expect TRUE (Z < a)"
    PRINT lowerZ > upper; " ! expect TRUE (z > A)"
    PRINT upper <= upperZ; " ! expect TRUE (A <= Z)"
ENDFUNC
BEGIN
    TestMixedCase()
END
RUN

! ===== TEST 7: Special Character Comparisons =====
NEW
FUNC TestSpecialChars()
    CHAR space = ' '
    CHAR exclaim = '!'
    CHAR at = '@'
    CHAR tilde = '~'
    PRINT "Special CHAR Tests:"
    PRINT space < exclaim; " ! expect TRUE"
    PRINT exclaim < at; " ! expect TRUE"
    PRINT at < tilde; " ! expect TRUE"
    PRINT tilde > space; " ! expect TRUE"
ENDFUNC
BEGIN
    TestSpecialChars()
END
RUN

! ===== TEST 8: CHR() Conversion Comparisons (Legal) =====
NEW
FUNC TestChrComparisons()
    CHAR c = 'M'
    BYTE b1 = 65  ! 'A'
    BYTE b2 = 90  ! 'Z'
    PRINT "CHR() Comparison Tests:"
    PRINT CHR(b1) < c; " ! expect TRUE (A < M)"
    PRINT CHR(b2) > c; " ! expect TRUE (Z > M)"
    PRINT CHR(b1) <= c; " ! expect TRUE (A <= M)"
    PRINT CHR(b2) >= c; " ! expect TRUE (Z >= M)"
    PRINT c > CHR(b1); " ! expect TRUE (M > A)"
ENDFUNC
BEGIN
    TestChrComparisons()
END
RUN

! ===== TEST 9: ASC() Conversion Comparisons (Legal) =====
NEW
FUNC TestAscComparisons()
    CHAR c1 = 'A'
    CHAR c2 = 'Z'
    INT i = 77   ! 'M'
    WORD w = 66  ! 'B'
    PRINT "ASC() Comparison Tests:"
    PRINT ASC(c1) < i; " ! expect TRUE (65 < 77)"
    PRINT ASC(c2) > i; " ! expect TRUE (90 > 77)"
    PRINT ASC(c1) < w; " ! expect TRUE (65 < 66)"
    PRINT ASC(c2) >= w; " ! expect TRUE (90 >= 66)"
ENDFUNC
BEGIN
    TestAscComparisons()
END
RUN

! ===== TEST 10: Complex Expression Comparisons =====
NEW
FUNC TestComplexExpr()
    CHAR c = 'G'
    BYTE b = 70  ! 'F'
    PRINT "Complex Expression Tests:"
    PRINT c > CHR(b); " ! expect TRUE (G > F)"
    PRINT ASC(c) > b; " ! expect TRUE (71 > 70)"
    ! Chained comparison
    CHAR c1 = 'B'
    CHAR c2 = 'D'
    PRINT c1 < c AND c < c2; " ! expect FALSE (B<G AND G<D)"
    CHAR c3 = 'A'
    CHAR c4 = 'Z'
    PRINT c3 < c AND c < c4; " ! expect TRUE (A<G AND G<Z)"
ENDFUNC
BEGIN
    TestComplexExpr()
END
RUN

! ===== TEST 11: Boundary Characters =====
NEW
FUNC TestBoundaries()
    CHAR nul = CHR(0)
    CHAR delete = CHR(127)
    CHAR ff = CHR(255)
    CHAR one = CHR(1)
    PRINT "Boundary CHAR Tests:"
    PRINT nul < one; " ! expect TRUE"
    PRINT one > nul; " ! expect TRUE"
    PRINT delete < ff; " ! expect TRUE"
    PRINT ff > delete; " ! expect TRUE"
    PRINT nul <= nul; " ! expect TRUE"
    PRINT ff >= ff; " ! expect TRUE"
ENDFUNC
BEGIN
    TestBoundaries()
END
RUN

! ===== TEST 12: String Indexing with Comparisons =====
NEW
FUNC TestStringIndex()
    STRING s = "HELLO"
    CHAR c1 = s[0]  ! 'H'
    CHAR c2 = s[1]  ! 'E'
    CHAR c3 = s[4]  ! 'O'
    PRINT "String Index Comparison:"
    PRINT c2 < c1; " ! expect TRUE (E < H)"
    PRINT c3 > c1; " ! expect TRUE (O > H)"
    PRINT c1 <= s[0]; " ! expect TRUE (H <= H)"
    PRINT s[2] = s[3]; " ! expect TRUE (L = L)"
ENDFUNC
BEGIN
    TestStringIndex()
END
RUN

! ===== TEST 13-17: Type Mismatch Errors =====
! Test these one at a time to catch expected errors

NEW
FUNC TestCharByteMismatch()
    CHAR c = 'A'
    BYTE b = 65
    PRINT "Direct CHAR < BYTE: "; c < b
ENDFUNC
BEGIN
    PRINT "Testing CHAR < BYTE (expect error):"
    TestCharByteMismatch()
END
RUN
! Expected: ?TYPE MISMATCH error

NEW
FUNC TestCharIntMismatch()
    CHAR c = 'Z'
    INT i = 90
    PRINT "Direct CHAR > INT: "; c > i
ENDFUNC
BEGIN
    PRINT "Testing CHAR > INT (expect error):"
    TestCharIntMismatch()
END
RUN
! Expected: ?TYPE MISMATCH error

NEW
FUNC TestCharWordMismatch()
    CHAR c = 'M'
    WORD w = 77
    PRINT "Direct CHAR <= WORD: "; c <= w
ENDFUNC
BEGIN
    PRINT "Testing CHAR <= WORD (expect error):"
    TestCharWordMismatch()
END
RUN
! Expected: ?TYPE MISMATCH error

NEW
FUNC TestCharStringMismatch()
    CHAR c = 'A'
    STRING s = "A"
    PRINT "Direct CHAR >= STRING: "; c >= s
ENDFUNC
BEGIN
    PRINT "Testing CHAR >= STRING (expect error):"
    TestCharStringMismatch()
END
RUN
! Expected: ?TYPE MISMATCH error

NEW
FUNC TestCharBitMismatch()
    CHAR c = 'T'
    BIT b = TRUE
    PRINT "Direct CHAR = BIT: "; c = b
ENDFUNC
BEGIN
    PRINT "Testing CHAR = BIT (expect error):"
    TestCharBitMismatch()
END
RUN
! Expected: ?TYPE MISMATCH error

! ===== TEST 18: VAR Type with CHAR Comparisons =====
NEW
FUNC TestVarChar()
    VAR v1 = 'A'
    VAR v2 = 'Z'
    CHAR c = 'M'
    PRINT "VAR(CHAR) Tests:"
    PRINT v1 < v2; " ! expect TRUE (A < Z)"
    PRINT v2 > v1; " ! expect TRUE (Z > A)"
    ! VAR to CHAR comparison
    PRINT v1 < c; " ! expect TRUE (A < M)"
    PRINT v2 > c; " ! expect TRUE (Z > M)"
    ! Change VAR type
    v1 = 100
    PRINT "After v1=100, type is now INT/WORD"
ENDFUNC
BEGIN
    TestVarChar()
END
RUN

! ===== TEST 19: Memory Check =====
NEW
MEM
PRINT "Memory check complete - no leaks expected"
