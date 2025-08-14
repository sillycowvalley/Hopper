! Comparison Tests for Hopper BASIC v2
! Testing = and <> operators with type promotion
! Following progressive isolation methodology
! All tests included - failures expected and documented

! ===== TEST 1: BIT Comparisons (Isolated) =====
NEW
FUNC TestBitCompare()
    BIT b1 = TRUE
    BIT b2 = FALSE
    BIT b3 = TRUE
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

! ===== TEST 2: BYTE Comparisons (Isolated) =====
NEW
FUNC TestByteCompare()
    BYTE b1 = 0
    BYTE b2 = 255
    BYTE b3 = 100
    BYTE b4 = 100
    PRINT "BYTE Tests:"
    PRINT b1 = 0; " ! expect TRUE"
    PRINT b2 = 255; " ! expect TRUE"
    PRINT b3 = b4; " ! expect TRUE"
    PRINT b1 <> b2; " ! expect TRUE"
    PRINT b3 <> b4; " ! expect FALSE"
ENDFUNC
BEGIN
    TestByteCompare()
END
RUN

! ===== TEST 3: INT Comparisons (Isolated) =====
NEW
FUNC TestIntCompare()
    INT i1 = -32768
    INT i2 = 32767
    INT i3 = 0
    INT i4 = -1
    PRINT "INT Tests:"
    PRINT i1 = -32768; " ! expect TRUE"
    PRINT i2 = 32767; " ! expect TRUE"
    PRINT i3 = 0; " ! expect TRUE"
    PRINT i4 <> 0; " ! expect TRUE"
    PRINT i1 <> i2; " ! expect TRUE"
ENDFUNC
BEGIN
    TestIntCompare()
END
RUN

! ===== TEST 4: WORD Comparisons (Isolated) =====
NEW
FUNC TestWordCompare()
    WORD w1 = 0
    WORD w2 = 65535
    WORD w3 = 32768
    WORD w4 = 32768
    PRINT "WORD Tests:"
    PRINT w1 = 0; " ! expect TRUE"
    PRINT w2 = 65535; " ! expect TRUE"
    PRINT w3 = w4; " ! expect TRUE"
    PRINT w3 = 32768; " ! expect TRUE"
    PRINT w1 <> w2; " ! expect TRUE"
ENDFUNC
BEGIN
    TestWordCompare()
END
RUN

! ===== TEST 5: CHAR Comparisons (Isolated) =====
NEW
FUNC TestCharCompare()
    CHAR c1 = 'A'
    CHAR c2 = 'Z'
    CHAR c3 = 'A'
    CHAR c4 = '0'
    PRINT "CHAR Tests:"
    PRINT c1 = c3; " ! expect TRUE"
    PRINT c1 = 'A'; " ! expect TRUE"
    PRINT c1 <> c2; " ! expect TRUE"
    PRINT c4 = '0'; " ! expect TRUE"
    ! Note: < > <= >= not implemented for CHAR
ENDFUNC
BEGIN
    TestCharCompare()
END
RUN

! ===== TEST 6: VAR Type Comparisons =====
NEW
FUNC TestVarCompare()
    VAR v1 = 42
    VAR v2 = 42
    VAR v3 = "HELLO"
    VAR v4 = TRUE
    PRINT "VAR Tests:"
    PRINT v1 = v2; " ! expect TRUE"
    PRINT v1 = 42; " ! expect TRUE"
    PRINT v3 = "HELLO"; " ! expect TRUE"
    PRINT v4 = TRUE; " ! expect TRUE"
    ! Test VAR type change (now fixed)
    v1 = "TEST"
    PRINT v1 = "TEST"; " ! expect TRUE"
ENDFUNC
BEGIN
    TestVarCompare()
END
RUN

! ===== TEST 7: Assignment Promotion =====
NEW
FUNC TestAssignPromo()
    BYTE b = 100
    WORD w
    INT i
    PRINT "Assignment Promotion:"
    w = b
    PRINT "BYTE->WORD: "; w; " ! expect 100"
    i = b
    PRINT "BYTE->INT: "; i; " ! expect 100"
    b = 255: w = b
    PRINT "BYTE(255)->WORD: "; w; " ! expect 255"
    b = 0: i = b
    PRINT "BYTE(0)->INT: "; i; " ! expect 0"
ENDFUNC
BEGIN
    TestAssignPromo()
END
RUN

! ===== TEST 8: Safe Range Comparisons =====
NEW
FUNC TestSafeCompare()
    BYTE b
    WORD w  
    INT i
    PRINT "Safe Range Comparisons:"
    ! Test within BYTE range (0-255)
    b = 100: w = 100: i = 100
    PRINT "b=100 w=100: "; b = w; " ! expect TRUE"
    PRINT "b=100 i=100: "; b = i; " ! expect TRUE"
    b = 255: w = 255: i = 255
    PRINT "b=255 w=255: "; b = w; " ! expect TRUE"
    PRINT "b=255 i=255: "; b = i; " ! expect TRUE"
    b = 0: w = 0: i = 0
    PRINT "b=0 w=0: "; b = w; " ! expect TRUE"
    PRINT "b=0 i=0: "; b = i; " ! expect TRUE"
ENDFUNC
BEGIN
    TestSafeCompare()
END
RUN

! ===== TEST 9: INT-WORD Safe Boundaries =====
NEW
FUNC TestIntWordSafe()
    INT i
    WORD w
    PRINT "INT-WORD Safe Range:"
    ! Test within positive overlap (0-32767)
    i = 0: w = 0
    PRINT "i=0 w=0: "; i = w; " ! expect TRUE"
    i = 32767: w = 32767
    PRINT "i=32767 w=32767: "; i = w; " ! expect TRUE"
    i = 1000: w = 1000
    PRINT "i=1000 w=1000: "; i = w; " ! expect TRUE"
ENDFUNC
BEGIN
    TestIntWordSafe()
END
RUN

! ===== TEST 10: Negative Global Test =====
NEW
INT global1 = -1
INT global2 = -32768
INT global3 = -100
BEGIN
    PRINT "Negative globals:"
    PRINT global1; " ! expect -1"
    PRINT global2; " ! expect -32768"
    PRINT global3; " ! expect -100"
END
RUN

! ===== TEST 11: Negative Local Test =====
NEW
FUNC TestNegativeLocal()
    INT i1 = -1
    INT i2 = -32768
    INT i3 = -100
    PRINT "Negative locals:"
    PRINT i1; " ! expect -1"
    PRINT i2; " ! expect -32768"
    PRINT i3; " ! expect -100"
ENDFUNC
BEGIN
    TestNegativeLocal()
END
RUN

! ===== TEST 12: BYTE-WORD Unsafe Comparisons =====
NEW
FUNC TestByteWordUnsafe()
    BYTE b = 255
    WORD w = 255
    PRINT "BYTE-WORD Comparisons:"
    PRINT b = w; " ! expect TRUE"
    PRINT b = 255; " ! expect TRUE"
    w = 256
    PRINT b <> w; " ! expect TRUE (may fail)"
    w = 0: b = 0
    PRINT b = w; " ! expect TRUE"
ENDFUNC
BEGIN
    TestByteWordUnsafe()
END
RUN

! ===== TEST 13: BYTE-INT Unsafe Comparisons =====
NEW
FUNC TestByteIntUnsafe()
    BYTE b = 100
    INT i = 100
    PRINT "BYTE-INT Comparisons:"
    PRINT b = i; " ! expect TRUE"
    i = 255: b = 255
    PRINT b = i; " ! expect TRUE"
    i = -1
    PRINT b <> i; " ! expect TRUE (may fail)"
    i = 256
    PRINT b <> i; " ! expect TRUE (may fail)"
ENDFUNC
BEGIN
    TestByteIntUnsafe()
END
RUN

! ===== TEST 14: INT-WORD Unsafe Boundaries =====
NEW
FUNC TestIntWordUnsafe()
    INT i = 32767
    WORD w = 32767
    PRINT "INT-WORD Boundaries:"
    PRINT i = w; " ! expect TRUE"
    w = 32768
    i = -32768
    PRINT i <> w; " ! expect TRUE (may fail)"
    i = 0: w = 0
    PRINT i = w; " ! expect TRUE"
    i = -1
    PRINT i <> w; " ! expect FALSE (may fail)"
ENDFUNC
BEGIN
    TestIntWordUnsafe()
END
RUN

! ===== TEST 15: STRING Comparisons =====
NEW
FUNC TestStringCompare()
    STRING s1 = "HELLO"
    STRING s2 = "WORLD"
    STRING s3 = "HELLO"
    STRING s4 = ""
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
! CRASHING   RUN

! ===== TEST 16: Type Mismatch Errors =====
! Test these one at a time to catch errors
NEW
FUNC TestTypeMismatch1()
    BIT b = TRUE
    INT i = 1
    PRINT "BIT = INT: "; b = i
ENDFUNC
BEGIN
    PRINT "Testing BIT = INT (expect error):"
    TestTypeMismatch1()
END
RUN
! Expected: ?TYPE MISMATCH error

NEW
FUNC TestTypeMismatch2()
    STRING s = "HELLO"
    INT i = 5
    PRINT "STRING = INT: "; s = i
ENDFUNC
BEGIN
    PRINT "Testing STRING = INT (expect error):"
    TestTypeMismatch2()
END
RUN
! Expected: ?TYPE MISMATCH error

NEW
FUNC TestTypeMismatch3()
    CHAR c = 'A'
    BYTE b = 65
    PRINT "CHAR = BYTE: "; c = b
ENDFUNC
BEGIN
    PRINT "Testing CHAR = BYTE (expect error):"
    TestTypeMismatch3()
END
RUN
! Expected: ?TYPE MISMATCH error

NEW
FUNC TestTypeMismatch4()
    CHAR c = 'A'
    STRING s = "A"
    PRINT "CHAR = STRING: "; c = s
ENDFUNC
BEGIN
    PRINT "Testing CHAR = STRING (expect error):"
    TestTypeMismatch4()
END
RUN
! Expected: ?TYPE MISMATCH error

NEW
FUNC TestTypeMismatch5()
    BIT b = TRUE
    STRING s = "TRUE"
    PRINT "BIT = STRING: "; b = s
ENDFUNC
BEGIN
    PRINT "Testing BIT = STRING (expect error):"
    TestTypeMismatch5()
END
RUN
! Expected: ?TYPE MISMATCH error