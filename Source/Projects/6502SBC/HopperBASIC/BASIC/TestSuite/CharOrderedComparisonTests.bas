CLS
! CHAR Ordered Comparison Test Suite for Hopper BASIC v3
! Tests < > <= >= operators for CHAR type with simplified type system
! Following progressive isolation methodology

NEW
MEM

! ===== TEST 1: Basic CHAR Ordered Comparisons =====
NEW
FUNC TestCharOrdered()
    VAR c1 = 'A'
    VAR c2 = 'B'
    VAR c3 = 'Z'
    VAR c4 = 'A'
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

! ===== TEST 2: CHAR Greater Than =====
NEW
FUNC TestCharGreater()
    VAR c1 = 'Z'
    VAR c2 = 'A'
    VAR c3 = 'M'
    VAR c4 = 'M'
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

! ===== TEST 3: CHAR Less Than or Equal =====
NEW
FUNC TestCharLessEqual()
    VAR c1 = 'A'
    VAR c2 = 'B'
    VAR c3 = 'A'
    PRINT "CHAR <= Tests:"
    PRINT c1 <= c2; " ! expect TRUE (A <= B)"
    PRINT c1 <= c3; " ! expect TRUE (A <= A)"
    PRINT c2 <= c1; " ! expect FALSE (B <= A)"
ENDFUNC
BEGIN
    TestCharLessEqual()
END
RUN

! ===== TEST 4: CHAR Greater Than or Equal =====
NEW
FUNC TestCharGreaterEqual()
    VAR c1 = 'Z'
    VAR c2 = 'Y'
    VAR c3 = 'Z'
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
    VAR d0 = '0'
    VAR d5 = '5'
    VAR d9 = '9'
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
    VAR upper = 'A'
    VAR lower = 'a'
    VAR upperZ = 'Z'
    PRINT "Mixed Case Tests:"
    PRINT upper < lower; " ! expect TRUE (A < a)"
    PRINT upperZ < lower; " ! expect TRUE (Z < a)"
    PRINT lower > upper; " ! expect TRUE (a > A)"
    PRINT upper <= upperZ; " ! expect TRUE (A <= Z)"
ENDFUNC
BEGIN
    TestMixedCase()
END
RUN

! ===== TEST 7: Special Characters =====
NEW
FUNC TestSpecialChars()
    VAR space = ' '
    VAR exclaim = '!'
    VAR at = '@'
    VAR tilde = '~'
    PRINT "Special CHAR Tests:"
    PRINT space < exclaim; " ! expect TRUE (space < !)"
    PRINT exclaim < at; " ! expect TRUE (! < @)"
    PRINT at < tilde; " ! expect TRUE (@ < ~)"
    PRINT tilde > space; " ! expect TRUE (~ > space)"
ENDFUNC
BEGIN
    TestSpecialChars()
END
RUN

! ===== TEST 8: CHR() Conversion Comparisons =====
NEW
FUNC TestChrComparisons()
    VAR c = 'M'
    VAR val1 = 65
    VAR val2 = 90
    PRINT "CHR() Comparison Tests:"
    PRINT CHR(val1) < c; " ! expect TRUE (CHR(65)=A < M)"
    PRINT CHR(val2) > c; " ! expect TRUE (CHR(90)=Z > M)"
    PRINT CHR(val1) <= c; " ! expect TRUE (A <= M)"
    PRINT CHR(val2) >= c; " ! expect TRUE (Z >= M)"
ENDFUNC
BEGIN
    TestChrComparisons()
END
RUN

! ===== TEST 9: ASC() Conversion Comparisons =====
NEW
FUNC TestAscComparisons()
    VAR c1 = 'A'
    VAR c2 = 'Z'
    VAR longVal = 77
    VAR longVal2 = 66
    PRINT "ASC() Comparison Tests:"
    PRINT ASC(c1) < longVal; " ! expect TRUE (ASC(A)=65 < 77)"
    PRINT ASC(c2) > longVal; " ! expect TRUE (ASC(Z)=90 > 77)"
    PRINT ASC(c1) < longVal2; " ! expect TRUE (65 < 66)"
    PRINT ASC(c2) >= longVal2; " ! expect TRUE (90 >= 66)"
ENDFUNC
BEGIN
    TestAscComparisons()
END
RUN

! ===== TEST 10: String Indexing Issue Test =====
NEW
FUNC TestStringIndex()
    VAR s = "HELLO"
    VAR c1 = s[0]
    VAR c2 = s[1]
    PRINT "String Index Test (known bug):"
    PRINT "s[0]="; c1; " s[1]="; c2; " ! may show ASCII values"
    PRINT c2 < c1; " ! expect TRUE if chars, may fail if LONG"
ENDFUNC
BEGIN
    TestStringIndex()
END
RUN

! ===== TEST 11: CHAR Array Elements =====
NEW
CHAR letters[4]
FUNC TestCharArray()
    letters[0] = 'A'
    letters[1] = 'C'
    letters[2] = 'B'
    letters[3] = 'D'
    PRINT "CHAR Array Tests:"
    PRINT letters[0] < letters[1]; " ! expect TRUE (A < C)"
    PRINT letters[1] > letters[2]; " ! expect TRUE (C > B)"
    PRINT letters[0] <= letters[0]; " ! expect TRUE (A <= A)"
    PRINT letters[3] >= letters[2]; " ! expect TRUE (D >= B)"
ENDFUNC
BEGIN
    TestCharArray()
END
RUN

! ===== TEST 12-14: Type Mismatch Errors =====
NEW
FUNC TestCharLongMismatch()
    VAR charVar = 'A'
    VAR longVar = 65
    PRINT "CHAR < LONG: "; charVar < longVar; " ! expect TYPE MISMATCH"
ENDFUNC
BEGIN
    TestCharLongMismatch()
END
RUN

NEW
FUNC TestCharStringMismatch()
    VAR c = 'A'
    VAR s = "A"
    PRINT "CHAR >= STRING: "; c >= s; " ! expect TYPE MISMATCH"
ENDFUNC
BEGIN
    TestCharStringMismatch()
END
RUN

NEW
FUNC TestCharBitMismatch()
    VAR c = 'T'
    VAR b = TRUE
    PRINT "CHAR = BIT: "; c = b; " ! expect TYPE MISMATCH"
ENDFUNC
BEGIN
    TestCharBitMismatch()
END
RUN

NEW
MEM
PRINT "CHAR comparison tests complete"
